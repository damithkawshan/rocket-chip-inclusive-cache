/*
 * Copyright 2019 SiFive, Inc.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You should have received a copy of LICENSE.Apache2 along with
 * this software. If not, you may obtain a copy at
 *
 *    https://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package sifive.blocks.inclusivecache

import chisel3._
import chisel3.experimental.dataview._
import chisel3.util._
import freechips.rocketchip.diplomacy.AddressSet
import freechips.rocketchip.tilelink._
import freechips.rocketchip.util._

class InclusiveCacheBankScheduler(params: InclusiveCacheParameters) extends Module
{

  // Calculate numBanks like in BankedStore
  val innerBytes = params.inner.manager.beatBytes
  val outerBytes = params.outer.manager.beatBytes
  val rowBytes = params.micro.portFactor * math.max(innerBytes, outerBytes)
  val numBanks = rowBytes / params.micro.writeBytes

  val io = IO(new Bundle {
    val in = Flipped(TLBundle(params.inner.bundle))
    val out = TLBundle(params.outer.bundle)
    // Way permissions
    val ways = Flipped(Vec(params.allClients, UInt(params.cache.ways.W)))
    val divs = Flipped(Vec(params.allClients, UInt((InclusiveCacheParameters.lfsrBits + 1).W)))
    // Control port
    val req = Flipped(Decoupled(new SinkXRequest(params)))

    //Bank disable ct
    val bankDisable = Input(UInt(4.W))

    val resp = Decoupled(new SourceXRequest(params))
    // Performance monitoring
    val perf = new Bundle {
      val access_valid = Output(Bool())
      val access_hit = Output(Bool())
    }
    // Saturation counters output (one per set, 8-bit each)
    val satCounters = Output(Vec(params.cache.sets, UInt(8.W)))
  })

  val sourceA = Module(new SourceA(params))
  val sourceB = Module(new SourceB(params))
  val sourceC = Module(new SourceC(params))
  val sourceD = Module(new SourceD(params))
  val sourceE = Module(new SourceE(params))
  val sourceX = Module(new SourceX(params))

  io.out.a <> sourceA.io.a
  io.out.c <> sourceC.io.c
  io.out.e <> sourceE.io.e
  io.in.b <> sourceB.io.b
  io.in.d <> sourceD.io.d
  io.resp <> sourceX.io.x

  val sinkA = Module(new SinkA(params))
  val sinkC = Module(new SinkC(params))
  val sinkD = Module(new SinkD(params))
  val sinkE = Module(new SinkE(params))
  val sinkX = Module(new SinkX(params))

  sinkA.io.a <> io.in.a
  sinkC.io.c <> io.in.c
  sinkE.io.e <> io.in.e
  sinkD.io.d <> io.out.d
  sinkX.io.x <> io.req
  val (aFirst, _, _, _) = params.outer.count(io.out.a)
  val (dFirst, _, _, _) = params.outer.count(io.out.d)
  val dIsReleaseAck = io.out.d.bits.opcode === TLMessages.ReleaseAck
  val blockDReadyForSourceHazard = io.out.d.valid && dFirst &&
    io.out.a.valid && aFirst &&
    (io.out.a.bits.source === io.out.d.bits.source) &&
    !dIsReleaseAck &&
    !io.out.a.ready
  sinkD.io.blockReady := blockDReadyForSourceHazard

  io.out.b.ready := true.B // disconnected

  val directory = Module(new Directory(params))
  val bankedStore = Module(new BankedStore(params))
  val requests = Module(new ListBuffer(ListBufferParameters(new QueuedRequest(params), 3*params.mshrs, params.secondary, false)))
  val mshrs = Seq.tabulate(params.mshrs) { i => Module(new MSHR(params, i)) }
  val abc_mshrs = mshrs.init.init
  val bc_mshr = mshrs.init.last
  val c_mshr = mshrs.last
  val nestedwb = Wire(new NestedWriteback(params))

  // Migration copy engine state (scheduler-controlled)
  val migrateIdle :: migrateReadReq :: migrateReadWait :: migrateWriteReq :: migrateDirDst :: migrateDirSrc :: migrateDone :: Nil = Enum(7)
  val migrateState = RegInit(migrateIdle)
  val migrationReq = Reg(new MigrateRequest(params))
  val migrationOwnerOH = RegInit(0.U(params.mshrs.W))
  val migrationBeat = RegInit(0.U(params.innerBeatBits.W))
  val migrationReadDelay = RegInit(0.U(2.W))
  val migrationReadData = Reg(UInt(params.inner.bundle.dataBits.W))
  val migrationBusy = migrateState =/= migrateIdle
  val migrateBlockBeats = params.cache.blockBytes / params.inner.manager.beatBytes
  val migrateLastBeat = (migrateBlockBeats - 1).U(params.innerBeatBits.W)
  val migrationDonePulse = WireDefault(false.B)

  // Deliver messages from Sinks to MSHRs
  mshrs.zipWithIndex.foreach { case (m, i) =>
    m.io.sinkc.valid := sinkC.io.resp.valid && sinkC.io.resp.bits.set === m.io.status.bits.physSet
    m.io.sinkd.valid := sinkD.io.resp.valid && sinkD.io.resp.bits.source === i.U
    m.io.sinke.valid := sinkE.io.resp.valid && sinkE.io.resp.bits.sink   === i.U
    m.io.sinkc.bits := sinkC.io.resp.bits
    m.io.sinkd.bits := sinkD.io.resp.bits
    m.io.sinke.bits := sinkE.io.resp.bits
    m.io.nestedwb := nestedwb
    // Connect partner lookup for migration - arbitrate among MSHRs
    m.io.partnerResult.valid := false.B
    m.io.partnerResult.bits := directory.io.partnerResult.bits
    // Default: no partner read grant
    m.io.partnerReadGrant := false.B
    // Default: migration not completed this cycle
    m.io.migrateDone := false.B
  }
  
  // Partner lookup arbitration - only one MSHR can use partner lookup at a time
  val partnerLookupReqs = mshrs.map(_.io.partnerLookup.valid)
  val partnerLookupGrant = PriorityEncoderOH(partnerLookupReqs)
  directory.io.partnerLookup.valid := !migrationBusy && partnerLookupReqs.reduce(_ || _)
  directory.io.partnerLookup.bits := Mux1H(partnerLookupGrant, mshrs.map(_.io.partnerLookup.bits))
  
  // Route partner result back to the requesting MSHR
  mshrs.zipWithIndex.foreach { case (m, i) =>
    when (partnerLookupGrant(i) && directory.io.partnerResult.valid) {
      m.io.partnerResult.valid := true.B
    }
  }

  // Partner read arbitration for SSBC secondary search
  val partnerReadReqs = mshrs.map(_.io.partnerRead.valid)
  val partnerReadOH = PriorityEncoderOH(Cat(partnerReadReqs.reverse))
  val partnerReadAny = partnerReadReqs.reduce(_ || _)
  val partnerReadFire = partnerReadAny && directory.io.ready && !migrationBusy
  val partnerReadBits = Mux1H(partnerReadOH, mshrs.map(_.io.partnerRead.bits))
  mshrs.zipWithIndex.foreach { case (m, i) =>
    when (partnerReadFire && partnerReadOH(i)) { m.io.partnerReadGrant := true.B }
  }

  // If the pre-emption BC or C MSHR have a matching set, the normal MSHR must be blocked
  val mshr_stall_abc = abc_mshrs.map { m =>
    (bc_mshr.io.status.valid && m.io.status.bits.physSet === bc_mshr.io.status.bits.physSet) ||
    ( c_mshr.io.status.valid && m.io.status.bits.physSet ===  c_mshr.io.status.bits.physSet)
  }
  val mshr_stall_bc =
    c_mshr.io.status.valid && bc_mshr.io.status.bits.physSet === c_mshr.io.status.bits.physSet
  val mshr_stall_c = false.B
  val mshr_stall = mshr_stall_abc :+ mshr_stall_bc :+ mshr_stall_c

  mshrs.zipWithIndex.foreach { case (m, i) =>
    when (migrationDonePulse && migrationOwnerOH(i)) {
      m.io.migrateDone := true.B
    }
  }


  val stall_abc = (mshr_stall_abc zip abc_mshrs) map { case (s, m) => s && m.io.status.valid }
  if (!params.lastLevel || !params.firstLevel)
    params.ccover(stall_abc.reduce(_||_), "SCHEDULER_ABC_INTERLOCK", "ABC MSHR interlocked due to pre-emption")
  if (!params.lastLevel)
    params.ccover(mshr_stall_bc && bc_mshr.io.status.valid, "SCHEDULER_BC_INTERLOCK", "BC MSHR interlocked due to pre-emption")

  // Consider scheduling an MSHR only if all the resources it requires are available
  val mshr_request = Cat((mshrs zip mshr_stall).map { case (m, s) =>
    m.io.schedule.valid && !s && !migrationBusy &&
      (sourceA.io.req.ready || !m.io.schedule.bits.a.valid) &&
      (sourceB.io.req.ready || !m.io.schedule.bits.b.valid) &&
      (sourceC.io.req.ready || !m.io.schedule.bits.c.valid) &&
      (sourceD.io.req.ready || !m.io.schedule.bits.d.valid) &&
      (sourceE.io.req.ready || !m.io.schedule.bits.e.valid) &&
      (sourceX.io.req.ready || !m.io.schedule.bits.x.valid) &&
      (directory.io.write.ready || !m.io.schedule.bits.dir.valid)
  }.reverse)

  // Round-robin arbitration of MSHRs
  val robin_filter = RegInit(0.U(params.mshrs.W))
  val robin_request = Cat(mshr_request, mshr_request & robin_filter)
  val mshr_selectOH2 = ~(leftOR(robin_request) << 1) & robin_request
  val mshr_selectOH = mshr_selectOH2(2*params.mshrs-1, params.mshrs) | mshr_selectOH2(params.mshrs-1, 0)
  val mshr_select = OHToUInt(mshr_selectOH)
  val schedule = Mux1H(mshr_selectOH, mshrs.map(_.io.schedule.bits))
  val scheduleTag = Mux1H(mshr_selectOH, mshrs.map(_.io.status.bits.tag))
  val scheduleSet = Mux1H(mshr_selectOH, mshrs.map(_.io.status.bits.set))

  // When an MSHR wins the schedule, it has lowest priority next time
  when (mshr_request.orR) { robin_filter := ~rightOR(mshr_selectOH) }

  // Fill in which MSHR sends the request
  schedule.a.bits.source := mshr_select
  schedule.c.bits.source := Mux(schedule.c.bits.opcode(1), mshr_select, 0.U) // only set for Release[Data] not ProbeAck[Data]
  schedule.d.bits.sink   := mshr_select

  sourceA.io.req.valid := !migrationBusy && schedule.a.valid
  sourceB.io.req.valid := !migrationBusy && schedule.b.valid
  sourceC.io.req.valid := !migrationBusy && schedule.c.valid
  sourceD.io.req.valid := !migrationBusy && schedule.d.valid
  sourceE.io.req.valid := !migrationBusy && schedule.e.valid
  sourceX.io.req.valid := !migrationBusy && schedule.x.valid

  sourceA.io.req.bits.viewAsSupertype(chiselTypeOf(schedule.a.bits)) := schedule.a.bits
  sourceB.io.req.bits.viewAsSupertype(chiselTypeOf(schedule.b.bits)) := schedule.b.bits
  sourceC.io.req.bits.viewAsSupertype(chiselTypeOf(schedule.c.bits)) := schedule.c.bits
  sourceD.io.req.bits.viewAsSupertype(chiselTypeOf(schedule.d.bits)) := schedule.d.bits
  sourceE.io.req.bits.viewAsSupertype(chiselTypeOf(schedule.e.bits)) := schedule.e.bits
  sourceX.io.req.bits.viewAsSupertype(chiselTypeOf(schedule.x.bits)) := schedule.x.bits

  val migrationDirWrite = Wire(Valid(new DirectoryWrite(params)))
  migrationDirWrite.valid := false.B
  migrationDirWrite.bits := 0.U.asTypeOf(new DirectoryWrite(params))
  directory.io.write.valid := Mux(migrationBusy, migrationDirWrite.valid, schedule.dir.valid)
  directory.io.write.bits.viewAsSupertype(chiselTypeOf(schedule.dir.bits)) := Mux(migrationBusy, migrationDirWrite.bits, schedule.dir.bits)

  // Forward meta-data changes from nested transaction completion
  val select_c  = mshr_selectOH(params.mshrs-1)
  val select_bc = mshr_selectOH(params.mshrs-2)
  nestedwb.set   := Mux(select_c, c_mshr.io.status.bits.set, bc_mshr.io.status.bits.set)
  nestedwb.tag   := Mux(select_c, c_mshr.io.status.bits.tag, bc_mshr.io.status.bits.tag)
  nestedwb.b_toN       := select_bc && bc_mshr.io.schedule.bits.dir.valid && bc_mshr.io.schedule.bits.dir.bits.data.state === MetaData.INVALID
  nestedwb.b_toB       := select_bc && bc_mshr.io.schedule.bits.dir.valid && bc_mshr.io.schedule.bits.dir.bits.data.state === MetaData.BRANCH
  nestedwb.b_clr_dirty := select_bc && bc_mshr.io.schedule.bits.dir.valid
  nestedwb.c_set_dirty := select_c  &&  c_mshr.io.schedule.bits.dir.valid && c_mshr.io.schedule.bits.dir.bits.data.dirty

  // Pick highest priority request
  val request = Wire(Decoupled(new FullRequest(params)))
  request.valid := directory.io.ready && (sinkA.io.req.valid || sinkX.io.req.valid || sinkC.io.req.valid)
  request.bits := Mux(sinkC.io.req.valid, sinkC.io.req.bits,
                  Mux(sinkX.io.req.valid, sinkX.io.req.bits, sinkA.io.req.bits))
  sinkC.io.req.ready := directory.io.ready && request.ready
  sinkX.io.req.ready := directory.io.ready && request.ready && !sinkC.io.req.valid
  sinkA.io.req.ready := directory.io.ready && request.ready && !sinkC.io.req.valid && !sinkX.io.req.valid

  // If no MSHR has been assigned to this set, we need to allocate one
  val setMatches = Cat(mshrs.map { m => m.io.status.valid && m.io.status.bits.physSet === request.bits.set }.reverse)
  val alloc = !setMatches.orR // NOTE: no matches also means no BC or C pre-emption on this set
  // If a same-set MSHR says that requests of this type must be blocked (for bounded time), do it
  val blockB = Mux1H(setMatches, mshrs.map(_.io.status.bits.blockB)) && request.bits.prio(1)
  val blockC = Mux1H(setMatches, mshrs.map(_.io.status.bits.blockC)) && request.bits.prio(2)
  // If a same-set MSHR says that requests of this type must be handled out-of-band, use special BC|C MSHR
  // ... these special MSHRs interlock the MSHR that said it should be pre-empted.
  val nestB  = Mux1H(setMatches, mshrs.map(_.io.status.bits.nestB))  && request.bits.prio(1)
  val nestC  = Mux1H(setMatches, mshrs.map(_.io.status.bits.nestC))  && request.bits.prio(2)
  // Prevent priority inversion; we may not queue to MSHRs beyond our level
  val prioFilter = Cat(request.bits.prio(2), !request.bits.prio(0), ~0.U((params.mshrs-2).W))
  val lowerMatches = setMatches & prioFilter
  // If we match an MSHR <= our priority that neither blocks nor nests us, queue to it.
  val queue = lowerMatches.orR && !nestB && !nestC && !blockB && !blockC

  if (!params.lastLevel) {
    params.ccover(request.valid && blockB, "SCHEDULER_BLOCKB", "Interlock B request while resolving set conflict")
    params.ccover(request.valid && nestB,  "SCHEDULER_NESTB", "Priority escalation from channel B")
  }
  if (!params.firstLevel) {
    params.ccover(request.valid && blockC, "SCHEDULER_BLOCKC", "Interlock C request while resolving set conflict")
    params.ccover(request.valid && nestC,  "SCHEDULER_NESTC", "Priority escalation from channel C")
  }
  params.ccover(request.valid && queue, "SCHEDULER_SECONDARY", "Enqueue secondary miss")

  // It might happen that lowerMatches has >1 bit if the two special MSHRs are in-use
  // We want to Q to the highest matching priority MSHR.
  val lowerMatches1 =
    Mux(lowerMatches(params.mshrs-1), 1.U << (params.mshrs-1),
    Mux(lowerMatches(params.mshrs-2), 1.U << (params.mshrs-2),
    lowerMatches))

  // If this goes to the scheduled MSHR, it may need to be bypassed
  // Alternatively, the MSHR may be refilled from a request queued in the ListBuffer
  val selected_requests = Cat(mshr_selectOH, mshr_selectOH, mshr_selectOH) & requests.io.valid
  val a_pop = selected_requests((0 + 1) * params.mshrs - 1, 0 * params.mshrs).orR
  val b_pop = selected_requests((1 + 1) * params.mshrs - 1, 1 * params.mshrs).orR
  val c_pop = selected_requests((2 + 1) * params.mshrs - 1, 2 * params.mshrs).orR
  val bypassMatches = (mshr_selectOH & lowerMatches1).orR &&
                      Mux(c_pop || request.bits.prio(2), !c_pop, Mux(b_pop || request.bits.prio(1), !b_pop, !a_pop))
  val may_pop = a_pop || b_pop || c_pop
  val bypass = request.valid && queue && bypassMatches
  val will_reload = schedule.reload && (may_pop || bypass)
  val will_pop = schedule.reload && may_pop && !bypass && !partnerReadFire

  params.ccover(mshr_selectOH.orR && bypass, "SCHEDULER_BYPASS", "Bypass new request directly to conflicting MSHR")
  params.ccover(mshr_selectOH.orR && will_reload, "SCHEDULER_RELOAD", "Back-to-back service of two requests")
  params.ccover(mshr_selectOH.orR && will_pop, "SCHEDULER_POP", "Service of a secondary miss")

  // Repeat the above logic, but without the fan-in
  mshrs.zipWithIndex.foreach { case (m, i) =>
    val sel = mshr_selectOH(i)
    m.io.schedule.ready := sel
    val a_pop = requests.io.valid(params.mshrs * 0 + i)
    val b_pop = requests.io.valid(params.mshrs * 1 + i)
    val c_pop = requests.io.valid(params.mshrs * 2 + i)
    val bypassMatches = lowerMatches1(i) &&
                        Mux(c_pop || request.bits.prio(2), !c_pop, Mux(b_pop || request.bits.prio(1), !b_pop, !a_pop))
    val may_pop = a_pop || b_pop || c_pop
    val bypass = request.valid && queue && bypassMatches
    val will_reload = m.io.schedule.bits.reload && (may_pop || bypass) && !partnerReadFire
    m.io.allocate.bits.viewAsSupertype(chiselTypeOf(requests.io.data)) := Mux(bypass, WireInit(new QueuedRequest(params), init = request.bits), requests.io.data)
    m.io.allocate.bits.set := m.io.status.bits.set
    m.io.allocate.bits.repeat := m.io.allocate.bits.tag === m.io.status.bits.tag
    m.io.allocate.valid := sel && will_reload
  }

  // Determine which of the queued requests to pop (supposing will_pop)
  val prio_requests = ~(~requests.io.valid | (requests.io.valid >> params.mshrs) | (requests.io.valid >> 2*params.mshrs))
  val pop_index = OHToUInt(Cat(mshr_selectOH, mshr_selectOH, mshr_selectOH) & prio_requests)
  requests.io.pop.valid := will_pop
  requests.io.pop.bits  := pop_index

  // Reload from the Directory if the next MSHR operation changes tags
  val lb_tag_mismatch = scheduleTag =/= requests.io.data.tag
  val mshr_uses_directory_assuming_no_bypass = schedule.reload && may_pop && lb_tag_mismatch && !partnerReadFire
  val mshr_uses_directory_for_lb = will_pop && lb_tag_mismatch
  val mshr_uses_directory = !partnerReadFire && will_reload && scheduleTag =/= Mux(bypass, request.bits.tag, requests.io.data.tag)

  // Is there an MSHR free for this request?
  val mshr_validOH = Cat(mshrs.map(_.io.status.valid).reverse)
  val mshr_free = (~mshr_validOH & prioFilter).orR

  // Fanout the request to the appropriate handler (if any)
  val bypassQueue = schedule.reload && bypassMatches
  val request_alloc_cases = !migrationBusy && !partnerReadFire && (
     (alloc && !mshr_uses_directory_assuming_no_bypass && mshr_free) ||
     (nestB && !mshr_uses_directory_assuming_no_bypass && !bc_mshr.io.status.valid && !c_mshr.io.status.valid) ||
     (nestC && !mshr_uses_directory_assuming_no_bypass && !c_mshr.io.status.valid))
  request.ready := !migrationBusy && !partnerReadFire && (request_alloc_cases || (queue && (bypassQueue || requests.io.push.ready)))
  val alloc_uses_directory = !migrationBusy && !partnerReadFire && request.valid && request_alloc_cases

  // When a request goes through, it will need to hit the Directory
  directory.io.read.valid := !migrationBusy && (partnerReadFire || mshr_uses_directory || alloc_uses_directory)
  directory.io.read.bits.set :=
    Mux(partnerReadFire, partnerReadBits.set,
      Mux(mshr_uses_directory_for_lb, scheduleSet, request.bits.set))
  directory.io.read.bits.tag :=
    Mux(partnerReadFire, partnerReadBits.tag,
      Mux(mshr_uses_directory_for_lb, requests.io.data.tag, request.bits.tag))
  directory.io.read.bits.source :=
    Mux(partnerReadFire, partnerReadBits.source,
      Mux(mshr_uses_directory_for_lb, requests.io.data.source, request.bits.source))

  // Enqueue the request if not bypassed directly into an MSHR
  requests.io.push.valid := request.valid && queue && !bypassQueue && !partnerReadFire && !migrationBusy
  requests.io.push.bits.data  := request.bits
  requests.io.push.bits.index := Mux1H(
    request.bits.prio, Seq(
      OHToUInt(lowerMatches1 << params.mshrs*0),
      OHToUInt(lowerMatches1 << params.mshrs*1),
      OHToUInt(lowerMatches1 << params.mshrs*2)))

  val mshr_insertOH = ~(leftOR(~mshr_validOH) << 1) & ~mshr_validOH & prioFilter
  (mshr_insertOH.asBools zip mshrs) map { case (s, m) =>
    when (request.valid && alloc && s && !mshr_uses_directory_assuming_no_bypass) {
      m.io.allocate.valid := true.B
      m.io.allocate.bits.viewAsSupertype(chiselTypeOf(request.bits)) := request.bits
      m.io.allocate.bits.repeat := false.B
    }
  }

  when (request.valid && nestB && !bc_mshr.io.status.valid && !c_mshr.io.status.valid && !mshr_uses_directory_assuming_no_bypass) {
    bc_mshr.io.allocate.valid := true.B
    bc_mshr.io.allocate.bits.viewAsSupertype(chiselTypeOf(request.bits)) := request.bits
    bc_mshr.io.allocate.bits.repeat := false.B
    assert (!request.bits.prio(0))
  }
  bc_mshr.io.allocate.bits.prio(0) := false.B

  when (request.valid && nestC && !c_mshr.io.status.valid && !mshr_uses_directory_assuming_no_bypass) {
    c_mshr.io.allocate.valid := true.B
    c_mshr.io.allocate.bits.viewAsSupertype(chiselTypeOf(request.bits)) := request.bits
    c_mshr.io.allocate.bits.repeat := false.B
    assert (!request.bits.prio(0))
    assert (!request.bits.prio(1))
  }
  c_mshr.io.allocate.bits.prio(0) := false.B
  c_mshr.io.allocate.bits.prio(1) := false.B

  // Fanout the result of the Directory lookup
  val dirTarget = Mux(alloc, mshr_insertOH, Mux(nestB,(BigInt(1) << (params.mshrs-2)).U,(BigInt(1) << (params.mshrs-1)).U))
  val dirReadSel = Mux(partnerReadFire, partnerReadOH,
                    Mux(mshr_uses_directory, mshr_selectOH,
                      Mux(alloc_uses_directory, dirTarget, 0.U)))
  val directoryFanout = params.dirReg(RegNext(dirReadSel))
  mshrs.zipWithIndex.foreach { case (m, i) =>
    m.io.directory.valid := directoryFanout(i)
    m.io.directory.bits := directory.io.result.bits
  }

  // Saturation counter update from MSHRs (at most one per cycle)
  val satUpdateReqs = mshrs.map(_.io.satUpdate.valid)
  val satUpdateOH = PriorityEncoderOH(satUpdateReqs)
  directory.io.satUpdate.valid := satUpdateReqs.reduce(_ || _)
  directory.io.satUpdate.bits := Mux1H(satUpdateOH, mshrs.map(_.io.satUpdate.bits))

  // MSHR response meta-data fetch
  sinkC.io.way :=
    Mux(bc_mshr.io.status.valid && bc_mshr.io.status.bits.physSet === sinkC.io.set,
      bc_mshr.io.status.bits.way,
      Mux1H(abc_mshrs.map(m => m.io.status.valid && m.io.status.bits.physSet === sinkC.io.set),
            abc_mshrs.map(_.io.status.bits.way)))
  sinkD.io.way := VecInit(mshrs.map(_.io.status.bits.way))(sinkD.io.source)
  sinkD.io.set := VecInit(mshrs.map(_.io.status.bits.set))(sinkD.io.source)

  // Beat buffer connections between components
  sinkA.io.pb_pop <> sourceD.io.pb_pop
  sourceD.io.pb_beat := sinkA.io.pb_beat
  sinkC.io.rel_pop <> sourceD.io.rel_pop
  sourceD.io.rel_beat := sinkC.io.rel_beat

  // BankedStore ports
  bankedStore.io.sinkC_adr <> sinkC.io.bs_adr
  bankedStore.io.sinkC_dat := sinkC.io.bs_dat
  bankedStore.io.sinkD_adr <> sinkD.io.bs_adr
  bankedStore.io.sinkD_dat := sinkD.io.bs_dat
  bankedStore.io.sourceC_adr <> sourceC.io.bs_adr
  bankedStore.io.sourceD_radr <> sourceD.io.bs_radr
  bankedStore.io.sourceD_wadr <> sourceD.io.bs_wadr
  bankedStore.io.sourceD_wdat := sourceD.io.bs_wdat
  bankedStore.io.bankDisable := io.bankDisable
  sourceC.io.bs_dat := bankedStore.io.sourceC_dat
  sourceD.io.bs_rdat := bankedStore.io.sourceD_rdat

  // Default migration copy ports (activated by migration FSM below)
  bankedStore.io.migrate_radr.valid := false.B
  bankedStore.io.migrate_radr.bits.noop := false.B
  bankedStore.io.migrate_radr.bits.way := 0.U
  bankedStore.io.migrate_radr.bits.set := 0.U
  bankedStore.io.migrate_radr.bits.beat := 0.U
  bankedStore.io.migrate_radr.bits.mask := ~0.U(params.innerMaskBits.W)
  bankedStore.io.migrate_wadr.valid := false.B
  bankedStore.io.migrate_wadr.bits.noop := false.B
  bankedStore.io.migrate_wadr.bits.way := 0.U
  bankedStore.io.migrate_wadr.bits.set := 0.U
  bankedStore.io.migrate_wadr.bits.beat := 0.U
  bankedStore.io.migrate_wadr.bits.mask := ~0.U(params.innerMaskBits.W)
  bankedStore.io.migrate_wdat.data := migrationReadData

  // SourceD data hazard interlock
  sourceD.io.evict_req := sourceC.io.evict_req
  sourceD.io.grant_req := sinkD  .io.grant_req
  sourceC.io.evict_safe := sourceD.io.evict_safe
  sinkD  .io.grant_safe := sourceD.io.grant_safe

  // Migration handling
  val migrationStart = !migrationBusy && mshr_selectOH.orR && schedule.migrate.valid
  when (migrationStart) {
    migrationReq := schedule.migrate.bits
    migrationOwnerOH := mshr_selectOH
    migrationBeat := 0.U
    migrationReadDelay := 0.U
    migrateState := migrateReadReq
    printf("[SSBC MIGRATE] START srcSet=%d srcWay=%d srcTag=0x%x srcState=%d srcDirty=%d -> dstSet=%d dstWay=%d\n",
           schedule.migrate.bits.srcSet, schedule.migrate.bits.srcWay, schedule.migrate.bits.srcTag,
           schedule.migrate.bits.srcState, schedule.migrate.bits.srcDirty,
           schedule.migrate.bits.dstSet, schedule.migrate.bits.dstWay)
  }

  val migratedDstEntry = Wire(new DirectoryEntry(params))
  migratedDstEntry.tag := migrationReq.srcTag
  migratedDstEntry.dirty := migrationReq.srcDirty
  migratedDstEntry.state := migrationReq.srcState
  migratedDstEntry.clients := migrationReq.srcClients
  migratedDstEntry.displaced := true.B
  migratedDstEntry.originSet := migrationReq.srcSet
  migratedDstEntry.source := migrationReq.srcSource

  val invalidEntry = Wire(new DirectoryEntry(params))
  invalidEntry.tag := 0.U
  invalidEntry.dirty := false.B
  invalidEntry.state := MetaData.INVALID
  invalidEntry.clients := 0.U
  invalidEntry.displaced := false.B
  invalidEntry.originSet := 0.U
  invalidEntry.source := 0.U

  switch (migrateState) {
    is (migrateReadReq) {
      bankedStore.io.migrate_radr.valid := true.B
      bankedStore.io.migrate_radr.bits.way := migrationReq.srcWay
      bankedStore.io.migrate_radr.bits.set := migrationReq.srcSet
      bankedStore.io.migrate_radr.bits.beat := migrationBeat
      when (bankedStore.io.migrate_radr.fire) {
        migrationReadDelay := 0.U
        migrateState := migrateReadWait
      }
    }
    is (migrateReadWait) {
      migrationReadDelay := migrationReadDelay + 1.U
      // BankedStore read data appears two cycles after accepted read address.
      when (migrationReadDelay === 1.U) {
        migrationReadData := bankedStore.io.migrate_rdat.data
        migrateState := migrateWriteReq
      }
    }
    is (migrateWriteReq) {
      bankedStore.io.migrate_wadr.valid := true.B
      bankedStore.io.migrate_wadr.bits.way := migrationReq.dstWay
      bankedStore.io.migrate_wadr.bits.set := migrationReq.dstSet
      bankedStore.io.migrate_wadr.bits.beat := migrationBeat
      when (bankedStore.io.migrate_wadr.fire) {
        when (migrationBeat === migrateLastBeat) {
          migrateState := migrateDirDst
        } .otherwise {
          migrationBeat := migrationBeat + 1.U
          migrateState := migrateReadReq
        }
      }
    }
    is (migrateDirDst) {
      migrationDirWrite.valid := true.B
      migrationDirWrite.bits.set := migrationReq.dstSet
      migrationDirWrite.bits.way := migrationReq.dstWay
      migrationDirWrite.bits.data := migratedDstEntry
      when (directory.io.write.ready) {
        migrateState := migrateDirSrc
      }
    }
    is (migrateDirSrc) {
      migrationDirWrite.valid := true.B
      migrationDirWrite.bits.set := migrationReq.srcSet
      migrationDirWrite.bits.way := migrationReq.srcWay
      migrationDirWrite.bits.data := invalidEntry
      when (directory.io.write.ready) {
        migrateState := migrateDone
      }
    }
    is (migrateDone) {
      migrationDonePulse := true.B
      migrateState := migrateIdle
      printf("[SSBC MIGRATE] DONE srcSet=%d srcWay=%d -> dstSet=%d dstWay=%d\n",
             migrationReq.srcSet, migrationReq.srcWay, migrationReq.dstSet, migrationReq.dstWay)
    }
  }

  // Performance monitoring - track directory access and hit/miss
  io.perf.access_valid := directory.io.result.valid
  io.perf.access_hit := directory.io.result.valid && directory.io.result.bits.hit

  // Saturation counters are stored in Directory module
  // Connect directory's saturation counters to scheduler's output port
  io.satCounters := directory.io.satCounters

  private def afmt(x: AddressSet) = s"""{"base":${x.base},"mask":${x.mask}}"""
  private def addresses = params.inner.manager.managers.flatMap(_.address).map(afmt _).mkString(",")
  private def setBits = params.addressMapping.drop(params.offsetBits).take(params.setBits).mkString(",")
  private def tagBits = params.addressMapping.drop(params.offsetBits + params.setBits).take(params.tagBits).mkString(",")
  private def simple = s""""reset":"${reset.pathName}","tagBits":[${tagBits}],"setBits":[${setBits}],"blockBytes":${params.cache.blockBytes},"ways":${params.cache.ways}"""
  def json: String = s"""{"addresses":[${addresses}],${simple},"directory":${directory.json},"subbanks":${bankedStore.json}}"""
}
