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
import chisel3.util._
import freechips.rocketchip.tilelink._
import TLPermissions._
import TLMessages._
import MetaData._
import chisel3.PrintableHelper
import chisel3.experimental.dataview._

class ScheduleRequest(params: InclusiveCacheParameters) extends InclusiveCacheBundle(params)
{
  val a = Valid(new SourceARequest(params))
  val b = Valid(new SourceBRequest(params))
  val c = Valid(new SourceCRequest(params))
  val d = Valid(new SourceDRequest(params))
  val e = Valid(new SourceERequest(params))
  val x = Valid(new SourceXRequest(params))
  val dir = Valid(new DirectoryWrite(params))
  val reload = Bool() // get next request via allocate (if any)
  // Migration support
  val migrate = Valid(new MigrateRequest(params))
}

// Migration request - copy data from one set/way to another
class MigrateRequest(params: InclusiveCacheParameters) extends InclusiveCacheBundle(params)
{
  val srcSet = UInt(params.setBits.W)
  val srcWay = UInt(params.wayBits.W)
  val srcTag = UInt(params.tagBits.W)
  val srcDirty = Bool()
  val srcState = UInt(params.stateBits.W)
  val srcClients = UInt(params.clientBits.W)
  val srcSource = UInt(params.inner.bundle.sourceBits.W)
  val dstSet = UInt(params.setBits.W)
  val dstWay = UInt(params.wayBits.W)
  // Info about the victim in destination set (needs to be evicted)
  val dstVictimTag = UInt(params.tagBits.W)
  val dstVictimDirty = Bool()
  val dstVictimValid = Bool()
}

class MSHRStatus(params: InclusiveCacheParameters) extends InclusiveCacheBundle(params)
{
  val set = UInt(params.setBits.W)
  val physSet = UInt(params.setBits.W)
  val tag = UInt(params.tagBits.W)
  val way = UInt(params.wayBits.W)
  val blockB = Bool()
  val nestB  = Bool()
  val blockC = Bool()
  val nestC  = Bool()
  val blockA = Bool()  // Bug #7: block channel-A requests when physSet != set (SSBC migration)
}

class NestedWriteback(params: InclusiveCacheParameters) extends InclusiveCacheBundle(params)
{
  val set = UInt(params.setBits.W)
  val tag = UInt(params.tagBits.W)
  val b_toN       = Bool() // nested Probes may unhit us
  val b_toB       = Bool() // nested Probes may demote us
  val b_clr_dirty = Bool() // nested Probes clear dirty
  val c_set_dirty = Bool() // nested Releases MAY set dirty
}

sealed trait CacheState
{
  val code = CacheState.index.U
  CacheState.index = CacheState.index + 1
}

object CacheState
{
  var index = 0
}

case object S_INVALID  extends CacheState
case object S_BRANCH   extends CacheState
case object S_BRANCH_C extends CacheState
case object S_TIP      extends CacheState
case object S_TIP_C    extends CacheState
case object S_TIP_CD   extends CacheState
case object S_TIP_D    extends CacheState
case object S_TRUNK_C  extends CacheState
case object S_TRUNK_CD extends CacheState

class MSHR(params: InclusiveCacheParameters, val id: Int) extends Module
{
  val io = IO(new Bundle {
    val allocate  = Flipped(Valid(new AllocateRequest(params))) // refills MSHR for next cycle
    val directory = Flipped(Valid(new DirectoryResult(params))) // triggers schedule setup
    val status    = Valid(new MSHRStatus(params))
    val schedule  = Decoupled(new ScheduleRequest(params))
    val sinkc     = Flipped(Valid(new SinkCResponse(params)))
    val sinkd     = Flipped(Valid(new SinkDResponse(params)))
    val sinke     = Flipped(Valid(new SinkEResponse(params)))
    val nestedwb  = Flipped(new NestedWriteback(params))
    // Partner lookup for migration
    val partnerLookup = Valid(new PartnerLookupRequest(params))
    val partnerResult = Flipped(Valid(new PartnerLookupResult(params)))
    // SSBC partner read (second search) request
    val partnerRead = Valid(new DirectoryRead(params))
    val partnerReadGrant = Input(Bool())
    // SSBC saturation counter update (final hit/miss)
    val satUpdate = Valid(new SatCounterUpdate(params))
    // Migration copy+directory update completion from scheduler
    val migrateDone = Input(Bool())
  })

  val request_valid = RegInit(false.B)
  val request = Reg(new FullRequest(params))
  val meta_valid = RegInit(false.B)
  val meta = Reg(new DirectoryResult(params))
  
  // SSBC secondary search state
  val ssbcEnabled = params.cache.ssbcEnabled.B
  val ssbcNeedPartner = RegInit(false.B)
  val ssbcWaitPartner = RegInit(false.B)
  val ssbcPrimary = Reg(new DirectoryResult(params))
  val ssbcK = params.cache.ways
  val ssbcSatMax = (2 * ssbcK - 1).U
  val ssbcSatLow = ssbcK.U

  // SSBC partner set helper
  def partnerSetOf(set: UInt): UInt = Cat(~set(params.setBits-1), set(params.setBits-2, 0))

  // Default SSBC outputs
  io.partnerRead.valid := ssbcNeedPartner
  io.partnerRead.bits.set := partnerSetOf(request.set)
  io.partnerRead.bits.tag := request.tag
  io.partnerRead.bits.logicalSet := request.set  // logical home is always the original request set
  io.partnerRead.bits.source := request.source
  io.satUpdate.valid := false.B
  io.satUpdate.bits.set := request.set
  io.satUpdate.bits.inc := false.B
  io.satUpdate.bits.dec := false.B

  // Define which states are valid
  when (meta_valid) {
    when (meta.state === INVALID) {
      assert (!meta.clients.orR)
      assert (!meta.dirty)
    }
    when (meta.state === BRANCH) {
      assert (!meta.dirty)
    }
    when (meta.state === TRUNK) {
      assert (meta.clients.orR)
      assert ((meta.clients & (meta.clients - 1.U)) === 0.U) // at most one
    }
    when (meta.state === TIP) {
      // noop
    }
  }

  // Completed transitions (s_ = scheduled), (w_ = waiting)
  val s_rprobe         = RegInit(true.B) // B
  val w_rprobeackfirst = RegInit(true.B)
  val w_rprobeacklast  = RegInit(true.B)
  val s_release        = RegInit(true.B) // CW w_rprobeackfirst
  val w_releaseack     = RegInit(true.B)
  val s_pprobe         = RegInit(true.B) // B
  val s_acquire        = RegInit(true.B) // A  s_release, s_pprobe [1]
  val s_flush          = RegInit(true.B) // X  w_releaseack
  val w_grantfirst     = RegInit(true.B)
  val w_grantlast      = RegInit(true.B)
  val w_grant          = RegInit(true.B) // first | last depending on wormhole
  val w_pprobeackfirst = RegInit(true.B)
  val w_pprobeacklast  = RegInit(true.B)
  val w_pprobeack      = RegInit(true.B) // first | last depending on wormhole
  val s_probeack       = RegInit(true.B) // C  w_pprobeackfirst (mutually exclusive with next two s_*)
  val s_grantack       = RegInit(true.B) // E  w_grantfirst ... CAN require both outE&inD to service outD
  val s_execute        = RegInit(true.B) // D  w_pprobeack, w_grant
  val w_grantack       = RegInit(true.B)
  val s_writeback      = RegInit(true.B) // W  w_*
  // For dirty evictions: directory slot must not be cleared until w_releaseack confirms
  // DRAM has accepted the dirty data, preventing a re-acquire from fetching stale data.
  val s_dir_evict      = RegInit(true.B) // DIR INVALID write (only after w_releaseack for dirty lines)
  
  // Migration states
  val s_migrate_lookup = RegInit(true.B)  // Request partner set lookup
  val w_migrate_lookup = RegInit(true.B)  // Wait for partner lookup result
  val s_migrate        = RegInit(true.B)  // Schedule migration
  val w_migrate_done   = RegInit(true.B)  // Wait for migration complete
  
  // Migration metadata
  val migrate_valid = RegInit(false.B)
  val migrate_partnerSet = Reg(UInt(params.setBits.W))
  val migrate_partnerWay = Reg(UInt(params.wayBits.W))
  val migrate_partnerVictimTag = Reg(UInt(params.tagBits.W))
  val migrate_partnerDisplaced = Reg(Bool())
  val migrate_partnerVictimOriginSet = Reg(UInt(params.setBits.W))
  val migrate_partnerVictimDirty = Reg(Bool())
  val migrate_partnerVictimValid = Reg(Bool())
  val migrate_partnerVictimClients = Reg(UInt(params.clientBits.W))
  val migrate_partnerVictimState = Reg(UInt(params.stateBits.W))
  val migrate_evict_partner = RegInit(false.B)

  // [1]: We cannot issue outer Acquire while holding blockB (=> outA can stall)
  // However, inB and outC are higher priority than outB, so s_release and s_pprobe
  // may be safely issued while blockB. Thus we must NOT try to schedule the
  // potentially stuck s_acquire with either of them (scheduler is all or none).

  // Meta-data that we discover underway
  val sink = Reg(UInt(params.outer.bundle.sinkBits.W))
  val gotT = Reg(Bool())
  val bad_grant = Reg(Bool())
  val probes_done = Reg(UInt(params.clientBits.W))
  val probes_toN = Reg(UInt(params.clientBits.W))
  val probes_noT = Reg(Bool())

  // SSBC secondary search control
  val dir_valid = io.directory.valid
  val primary_need_partner = dir_valid && !ssbcNeedPartner && !ssbcWaitPartner &&
    ssbcEnabled && !io.directory.bits.hit && io.directory.bits.scBit

  // when (dir_valid) {
  //   printf("[MSHR %d] DirectoryResult: set=%d tag=0x%x way=%d hit=%d state=%d clients=0x%x dirty=%d displaced=%d originSet=%d partnerSet=%d partnerWay=%d scBit=%d\n",
  //     id.U,
  //     io.directory.bits.set,
  //     io.directory.bits.tag,
  //     io.directory.bits.way,
  //     io.directory.bits.hit,
  //     io.directory.bits.state,
  //     io.directory.bits.clients,
  //     io.directory.bits.dirty,
  //     io.directory.bits.displaced,
  //     io.directory.bits.originSet,
  //     io.directory.bits.partnerSet,
  //     io.directory.bits.partnerWay,
  //     io.directory.bits.scBit
  //   )
  // }

  when (primary_need_partner) {
    ssbcPrimary := io.directory.bits
    ssbcNeedPartner := true.B
    printf("[InclusiveCache][SSBC MSHR %d] PRIMARY_MISS origSet=%d origWay=%d origTag=0x%x displaced=%d scBit=%d -> partnerSet=%d lookup\n",
           id.U, io.directory.bits.set, io.directory.bits.way, io.directory.bits.tag, io.directory.bits.displaced, io.directory.bits.scBit, io.directory.bits.partnerSet)
  }
  when (dir_valid && !ssbcNeedPartner && !ssbcWaitPartner && ssbcEnabled && !io.directory.bits.hit && !io.directory.bits.scBit) {
    printf("[InclusiveCache][SSBC MSHR %d] PRIMARY_MISS origSet=%d origWay=%d origTag=0x%x displaced=%d scBit=%d (no secondary search)\n",
           id.U, io.directory.bits.set, io.directory.bits.way, io.directory.bits.tag, io.directory.bits.displaced, io.directory.bits.scBit)
  }

  when (dir_valid && !ssbcNeedPartner && !ssbcWaitPartner && ssbcEnabled && io.directory.bits.hit) {
    printf("[InclusiveCache][SSBC MSHR %d] PRIMARY_HIT origSet=%d origWay=%d origTag=0x%x\n",
           id.U, io.directory.bits.set, io.directory.bits.way, io.directory.bits.tag)
  }

  when (ssbcNeedPartner && io.partnerReadGrant) {
    ssbcNeedPartner := false.B
    ssbcWaitPartner := true.B
  }
  // DEBUG: print when ssbcNeedPartner is asserted but partnerReadGrant hasn't fired
  when (ssbcNeedPartner && !io.partnerReadGrant) {
    printf("[InclusiveCache][SSBC MSHR %d] PARTNER_READ_WAIT partnerRead.valid=%d partnerReadGrant=%d set=%d tag=0x%x\n",
           id.U, io.partnerRead.valid, io.partnerReadGrant, request.set, request.tag)
  }

  val secondary_hit = ssbcWaitPartner && dir_valid &&
    io.directory.bits.hit && io.directory.bits.displaced &&
    (io.directory.bits.originSet === ssbcPrimary.set)

  val dir_final_valid = dir_valid && (ssbcWaitPartner || !primary_need_partner)
  val dir_final = Wire(new DirectoryResult(params))
  dir_final := io.directory.bits
  when (ssbcWaitPartner && dir_valid) {
    dir_final := Mux(secondary_hit, io.directory.bits, ssbcPrimary)
    dir_final.hit := secondary_hit
  }
  when (ssbcWaitPartner && dir_valid) {
    ssbcWaitPartner := false.B
    when (secondary_hit) {
      printf("[InclusiveCache][SSBC MSHR %d] SECONDARY_HIT origSet=%d partnerSet=%d partnerWay=%d partnerTag=0x%x\n",
             id.U, ssbcPrimary.set, io.directory.bits.set, io.directory.bits.way, io.directory.bits.tag)
    } .otherwise {
      printf("[InclusiveCache][SSBC MSHR %d] SECONDARY_MISS origSet=%d partnerSet=%d partnerWay=%d partnerLineDisplaced=%d partnerTag=0x%x\n",
             id.U, ssbcPrimary.set, io.directory.bits.set, io.directory.bits.way, io.directory.bits.displaced, io.directory.bits.tag)
    }
  }

  // Saturation counter update after final hit/miss outcome
  when (dir_final_valid && ssbcEnabled) {
    io.satUpdate.valid := true.B
    io.satUpdate.bits.set := request.set
    io.satUpdate.bits.dec := dir_final.hit
    io.satUpdate.bits.inc := !dir_final.hit
  }

  // SSBC displacement decision (controller-side)
  // Bug #3 fix: never re-migrate a displaced line. A displaced line is already sitting in its
  // partner set from a prior migration; sending it back to originSet creates ping-pong and
  // deadlocks when originSet's MSHR slot is busy. Displaced victims must take the normal
  // s_release → DRAM eviction path.
  val shouldMigrate = ssbcEnabled && !dir_final.hit && (dir_final.state =/= INVALID) && (dir_final.currentSat > 0.U) &&
                      (dir_final.currentSat >= dir_final.partnerSat) &&
                      !dir_final.displaced
                      // (dir_final.currentSat >= ssbcSatMax) &&
                      // (dir_final.partnerSat < ssbcSatLow)
  // Cheap-only migration: only migrate when source has no inner-cache clients (no probe needed).
  // Bug #6 fix: if the source line is held by an L1 (clients != 0) we would need to probe it
  // before migration, which is too expensive and error-prone. Fall back to normal eviction.
  val srcCheap  = (dir_final.clients === 0.U)
  val doMigrate = shouldMigrate && srcCheap

  // When a nested transaction completes, update our meta data
  when (meta_valid && meta.state =/= INVALID &&
        io.nestedwb.set === request.set && io.nestedwb.tag === meta.tag) {
    when (io.nestedwb.b_clr_dirty) { meta.dirty := false.B }
    when (io.nestedwb.c_set_dirty) { meta.dirty := true.B }
    when (io.nestedwb.b_toB) { meta.state := BRANCH }
    when (io.nestedwb.b_toN) { meta.hit := false.B }
  }

  // Scheduler status
  io.status.valid := request_valid
  io.status.bits.set    := request.set
  io.status.bits.physSet := Mux(migrate_evict_partner, migrate_partnerSet, Mux(meta_valid, meta.set, request.set))
  io.status.bits.tag    := request.tag
  io.status.bits.way    := Mux(migrate_evict_partner, migrate_partnerWay, meta.way)
  io.status.bits.blockB := !meta_valid || ((!w_releaseack || !w_rprobeacklast || !w_pprobeacklast) && !w_grantfirst)
  io.status.bits.nestB  := meta_valid && w_releaseack && w_rprobeacklast && w_pprobeacklast && !w_grantfirst
  // The above rules ensure we will block and not nest an outer probe while still doing our
  // own inner probes. Thus every probe wakes exactly one MSHR.
  io.status.bits.blockC := !meta_valid
  io.status.bits.nestC  := meta_valid && (!w_rprobeackfirst || !w_pprobeackfirst || !w_grantfirst)
  // Bug #7: block A-channel requests from matching this MSHR when physSet != set.
  // During SSBC migration, physSet temporarily points to the partner set; queuing an
  // A-channel request here would overwrite its set field with the wrong logical set.
  io.status.bits.blockA := io.status.bits.physSet =/= io.status.bits.set
  // The w_grantfirst in nestC is necessary to deal with:
  //   acquire waiting for grant, inner release gets queued, outer probe -> inner probe -> deadlock
  // ... this is possible because the release+probe can be for same set, but different tag

  // We can only demand: block, nest, or queue
  assert (!io.status.bits.nestB || !io.status.bits.blockB)
  assert (!io.status.bits.nestC || !io.status.bits.blockC)

  // Scheduler requests
  val no_wait = w_rprobeacklast && w_releaseack && w_grantlast && w_pprobeacklast && w_grantack && w_migrate_done
  val partner_evict_done = migrate_evict_partner && w_rprobeacklast && w_releaseack
  when (partner_evict_done) {
    migrate_evict_partner := false.B
    // Partner victim eviction is complete; return to normal (post-evict) flow.
    s_rprobe := true.B
    s_release := true.B
    w_rprobeackfirst := true.B
    w_rprobeacklast := true.B
    w_releaseack := true.B
    // FIX: clear s_dir_evict so the s_acquire guard (s_release && s_pprobe && s_migrate && s_dir_evict)
    // can be satisfied. The partner victim's dirty writeback was already handled by this eviction's
    // own w_releaseack; leaving s_dir_evict=false here permanently blocks s_acquire (deadlock).
    s_dir_evict := true.B
    printf("[InclusiveCache][SSBC MSHR %d] PARTNER_EVICT_DONE partnerSet=%d partnerWay=%d partnerTag=0x%x\n",
           id.U, migrate_partnerSet, migrate_partnerWay, migrate_partnerVictimTag)
  }
  // Acquire is only legal once migration (if any) has actually completed.
  // Also blocked until s_dir_evict: for dirty evictions we must not issue the outer Acquire
  // before the directory slot has been cleared (which itself waits for w_releaseack).
  io.schedule.bits.a.valid := !s_acquire && s_release && s_pprobe && s_migrate && w_migrate_done && s_dir_evict
  io.schedule.bits.b.valid := !s_rprobe || !s_pprobe
  io.schedule.bits.c.valid := (!s_release && w_rprobeackfirst && !request.control.invalidate && (!migrate_valid || migrate_evict_partner)) || (!s_probeack && w_pprobeackfirst)
  io.schedule.bits.d.valid := !s_execute && w_pprobeack && w_grant
  io.schedule.bits.e.valid := !s_grantack && w_grantfirst
  io.schedule.bits.x.valid := (!s_flush && w_releaseack && !request.control.invalidate) || (!s_flush && w_rprobeackfirst && request.control.invalidate)
  // For dirty evictions the DIR INVALID write is deferred until w_releaseack so that
  // a directory slot is not visible as free while the dirty Release is still in flight.
  // Clean evictions (w_releaseack already true at scheduling time) are unaffected.
  val dirEvictReady = w_rprobeackfirst && w_releaseack && (!migrate_valid || migrate_evict_partner)
  io.schedule.bits.dir.valid := (!s_release && dirEvictReady) || (!s_dir_evict && w_releaseack && s_release) || (!s_writeback && no_wait)
  io.schedule.bits.reload := no_wait
  
  // Migration schedule - when we have migration info and normal release is done
  io.schedule.bits.migrate.valid := !s_migrate && w_migrate_lookup && migrate_valid && !migrate_evict_partner
  io.schedule.bits.migrate.bits.srcSet := request.set
  io.schedule.bits.migrate.bits.srcWay := meta.way
  io.schedule.bits.migrate.bits.srcTag := meta.tag
  io.schedule.bits.migrate.bits.srcDirty := meta.dirty
  io.schedule.bits.migrate.bits.srcState := meta.state
  io.schedule.bits.migrate.bits.srcClients := meta.clients
  io.schedule.bits.migrate.bits.srcSource := meta.source
  io.schedule.bits.migrate.bits.dstSet := migrate_partnerSet
  io.schedule.bits.migrate.bits.dstWay := migrate_partnerWay
  io.schedule.bits.migrate.bits.dstVictimTag := migrate_partnerVictimTag
  io.schedule.bits.migrate.bits.dstVictimDirty := migrate_partnerVictimDirty
  io.schedule.bits.migrate.bits.dstVictimValid := migrate_partnerVictimValid
  
  // Partner lookup request - request partner set info when migration is needed.
  // Guard with !w_migrate_lookup so we don't re-issue after the result has arrived.
  io.partnerLookup.valid := !s_migrate_lookup && !w_migrate_lookup && meta_valid && migrate_valid
  io.partnerLookup.bits.set := meta.partnerSet

  // DEBUG: print state when MSHR has migration but partnerLookup isn't firing
  // when (request_valid && migrate_valid && !w_migrate_done) {
  //   printf("[InclusiveCache][SSBC MSHR %d] MIGRATE_STATE s_ml=%d w_ml=%d meta_v=%d s_m=%d w_md=%d evict_p=%d s_acq=%d s_dir=%d s_rel=%d w_relack=%d\n",
  //          id.U, s_migrate_lookup, w_migrate_lookup, meta_valid,
  //          s_migrate, w_migrate_done, migrate_evict_partner, s_acquire, s_dir_evict, s_release, w_releaseack)
  // }

  io.schedule.valid := io.schedule.bits.a.valid || io.schedule.bits.b.valid || io.schedule.bits.c.valid ||
                       io.schedule.bits.d.valid || io.schedule.bits.e.valid || io.schedule.bits.x.valid ||
                       io.schedule.bits.dir.valid || io.schedule.bits.migrate.valid

  // Schedule completions
  when (io.schedule.ready) {
                                                            s_rprobe     := true.B
    // For normal eviction and partner-victim eviction, release is done once scheduled.
    when (w_rprobeackfirst && (!migrate_valid || migrate_evict_partner)) { s_release := true.B }
    // For migration (without partner-victim release), advance to partner lookup stage.
    when (w_rprobeackfirst && migrate_valid && !migrate_evict_partner)   { s_migrate_lookup := true.B }
                                                            s_pprobe     := true.B
    when (s_release && s_pprobe && s_migrate && s_dir_evict) { s_acquire  := true.B }
    when (w_releaseack && !request.control.invalidate)    { s_flush      := true.B }
    when (w_rprobeackfirst && request.control.invalidate) { s_flush      := true.B } // Invalidate only requires probe ack back
    when (w_pprobeackfirst)                               { s_probeack   := true.B }
    when (w_grantfirst)                                   { s_grantack   := true.B }
    when (w_pprobeack && w_grant)                         { s_execute    := true.B }
    when (no_wait)                                        { s_writeback  := true.B }
    // Guard with !no_wait: on the writeback (completion) cycle, migrate_valid is still true
    // (cleared later in the same when-block) so migrate.valid can spuriously fire and
    // pre-pollute s_migrate/w_migrate_done for the next incoming allocation.
    when (io.schedule.bits.migrate.valid && !no_wait)     { s_migrate    := true.B }
    // Keep waiting until scheduler reports migration copy+directory updates completed.
    when (io.schedule.bits.migrate.valid && !no_wait)     { w_migrate_done := false.B }
    // Once the deferred DIR INVALID write fires (after w_releaseack), the slot is free.
    when (!s_dir_evict && w_releaseack && s_release)      { s_dir_evict  := true.B }
    // Await the next operation
    when (no_wait) {
      request_valid := false.B
      meta_valid := false.B
      migrate_valid := false.B
      ssbcNeedPartner := false.B
      ssbcWaitPartner := false.B
      migrate_evict_partner := false.B
    }
  }

  when (io.migrateDone) {
    w_migrate_done := true.B
    printf("[InclusiveCache][SSBC MSHR %d] MIGRATE_DONE origSet=%d origWay=%d partnerSet=%d partnerWay=%d\n",
           id.U, request.set, meta.way, migrate_partnerSet, migrate_partnerWay)
  }
  
  // Handle partner lookup result
  when (io.partnerResult.valid && !w_migrate_lookup) {
    // Always capture the partner lookup metadata into registers
    migrate_partnerWay             := io.partnerResult.bits.way
    migrate_partnerVictimTag       := io.partnerResult.bits.victimTag
    migrate_partnerVictimDirty     := io.partnerResult.bits.victimDirty
    migrate_partnerVictimValid     := io.partnerResult.bits.victimValid
    migrate_partnerVictimClients   := io.partnerResult.bits.victimClients
    migrate_partnerDisplaced       := io.partnerResult.bits.victimDisplacedBit
    migrate_partnerVictimOriginSet := io.partnerResult.bits.victimOriginSet
    migrate_partnerVictimState     := io.partnerResult.bits.victimState
    printf("[InclusiveCache][SSBC MSHR %d] PARTNER_LOOKUP partnerSet=%d partnerWay=%d partnerTag=0x%x [isDisplacedLine=%d from Set=%d] dirty=%d valid=%d\n",
           id.U, migrate_partnerSet, io.partnerResult.bits.way, io.partnerResult.bits.victimTag,
           io.partnerResult.bits.victimDisplacedBit, io.partnerResult.bits.victimOriginSet,
           io.partnerResult.bits.victimDirty, io.partnerResult.bits.victimValid)

    // Cheap-only gate: partner victim is cheap if the slot is empty OR has no L1 clients.
    // Bug #6 fix: never migrate into a slot whose victim requires inner-cache probes.
    val partnerCheap = (!io.partnerResult.bits.victimValid) ||
                       (io.partnerResult.bits.victimClients === 0.U)
    printf("[InclusiveCache][SSBC MSHR %d] PARTNER_CHEAP srcClients=0x%x partnerCheap=%d victimValid=%d victimClients=0x%x victimDirty=%d\n",
           id.U, meta.clients, partnerCheap, io.partnerResult.bits.victimValid,
           io.partnerResult.bits.victimClients, io.partnerResult.bits.victimDirty)

    when (!partnerCheap) {
      // CANCEL migration: reset all migration FSM state so we don't stall.
      // Fall back to normal source-set eviction (the original victim before migration was chosen).
      w_migrate_lookup   := true.B   // mark lookup done (so FSM doesn't wait forever)
      migrate_valid      := false.B
      migrate_evict_partner := false.B
      s_migrate_lookup   := true.B
      s_migrate          := true.B
      w_migrate_done     := true.B
      // Re-arm the normal eviction path for the source-set victim.
      // (Migration path had set s_release/w_releaseack to true at allocation time;
      //  we now revert to the non-migration eviction settings.)
      s_release          := false.B
      w_releaseack       := false.B
      when (meta.dirty) { s_dir_evict := false.B }
      // Re-check whether source victim needs inner-cache probes.
      when ((!params.firstLevel).B && (meta.clients =/= 0.U)) {
        s_rprobe         := false.B
        w_rprobeackfirst := false.B
        w_rprobeacklast  := false.B
      }
      printf("[InclusiveCache][SSBC MSHR %d] MIGRATE_CANCEL (partnerCheap=0) srcSet=%d srcWay=%d -> fallback normal evict\n",
             id.U, request.set, meta.way)
    } .otherwise {
      // partnerCheap: proceed with migration
      w_migrate_lookup := true.B

      // Step 1: handle partner victim eviction before migration (only if slot occupied)
      when (io.partnerResult.bits.victimValid) {
        migrate_evict_partner := true.B
        printf("[InclusiveCache][SSBC MSHR %d] PARTNER_EVICT_START partnerSet=%d partnerWay=%d partnerTag=0x%x [isDisplacedLine=%d from Set=%d] dirty=%d clients=0x%x state=%d\n",
               id.U, migrate_partnerSet, io.partnerResult.bits.way, io.partnerResult.bits.victimTag,
               io.partnerResult.bits.victimDisplacedBit, io.partnerResult.bits.victimOriginSet,
               io.partnerResult.bits.victimDirty, io.partnerResult.bits.victimClients,
               io.partnerResult.bits.victimState)
        // Schedule release if dirty; otherwise skip release ack wait.
        // (victimClients === 0 guaranteed by partnerCheap, so no inner probes needed.)
        s_release    := false.B
        w_releaseack := !io.partnerResult.bits.victimDirty
        // No inner probes needed (partnerCheap guarantees victimClients === 0)
        s_rprobe         := true.B
        w_rprobeackfirst := true.B
        w_rprobeacklast  := true.B
      }
    }
  }

  // Resulting meta-data
  val final_meta_writeback = WireInit(meta)

  val req_clientBit = params.clientBit(request.source)
  val req_needT = needT(request.opcode, request.param)
  val req_acquire = request.opcode === AcquireBlock || request.opcode === AcquirePerm
  val meta_no_clients = !meta.clients.orR
  val req_promoteT = req_acquire && Mux(meta.hit, meta_no_clients && meta.state === TIP, gotT)

  // Eviction metadata (normal vs partner victim)
  // For normal eviction, use the logical meta.set (not request.set) so displaced-line
  // evictions probe/release against the correct logical address.
  val evictPhysSet  = Mux(migrate_evict_partner, migrate_partnerSet,  meta.set)
  val evictTag      = Mux(migrate_evict_partner, migrate_partnerVictimTag,      meta.tag)
  val evictWay      = Mux(migrate_evict_partner, migrate_partnerWay,            meta.way)
  val evictDirty    = Mux(migrate_evict_partner, migrate_partnerVictimDirty,    meta.dirty)
  val evictState    = Mux(migrate_evict_partner, migrate_partnerVictimState,     meta.state)
  val evictClients  = Mux(migrate_evict_partner, migrate_partnerVictimClients,  meta.clients)
  // The logical (address) set for probes/releases: if the evicted line is displaced,
  // its logical owner is meta.originSet, not the physical cache set.
  // val evictSet      = Mux(migrate_evict_partner, migrate_partnerSet,
  //                       Mux(meta.displaced,       meta.originSet,    meta.set))
  val evictSet = Mux(migrate_evict_partner,
                 Mux(migrate_partnerDisplaced,
                     migrate_partnerVictimOriginSet,  // displaced partner victim → use its logical home
                     migrate_partnerSet),             // non-displaced → physSet == logicalSet
                 Mux(meta.displaced,
                     meta.originSet,                  // displaced self-victim (already correct)
                     meta.set))

  when (request.prio(2) && (!params.firstLevel).B) { // always a hit
    final_meta_writeback.dirty   := meta.dirty || request.opcode(0)
    final_meta_writeback.state   := Mux(request.param =/= TtoT && meta.state === TRUNK, TIP, meta.state)
    final_meta_writeback.clients := meta.clients & ~Mux(isToN(request.param), req_clientBit, 0.U)
    final_meta_writeback.hit     := true.B // chained requests are hits
  } .elsewhen (request.control.flush && params.control.B) { // request.prio(0)
    when (meta.hit) {
      final_meta_writeback.dirty   := false.B
      final_meta_writeback.state   := INVALID
      final_meta_writeback.clients := meta.clients & ~probes_toN
    }
    final_meta_writeback.hit := false.B
    final_meta_writeback.source := 0.U
  } .otherwise {
    final_meta_writeback.dirty := (meta.hit && meta.dirty) || !request.opcode(2)
    final_meta_writeback.state := Mux(req_needT,
                                    Mux(req_acquire, TRUNK, TIP),
                                    Mux(!meta.hit, Mux(gotT, Mux(req_acquire, TRUNK, TIP), BRANCH),
                                      MuxCase(0.U(params.stateBits.W), Seq(
                                        (meta.state === INVALID) -> BRANCH,
                                        (meta.state === BRANCH)  -> BRANCH,
                                        (meta.state === TRUNK)   -> TIP,
                                        (meta.state === TIP)     -> Mux(meta_no_clients && req_acquire, TRUNK, TIP)))))
    final_meta_writeback.clients := Mux(meta.hit, meta.clients & ~probes_toN, 0.U) |
                                    Mux(req_acquire, req_clientBit, 0.U)
    final_meta_writeback.tag := request.tag
    final_meta_writeback.hit := true.B
    // Preserve displaced metadata on hits so SSBC secondary lookup remains valid.
    // Fresh allocations are native to the request set.
    final_meta_writeback.displaced := Mux(meta.hit, meta.displaced, false.B)
    final_meta_writeback.originSet := Mux(meta.hit && meta.displaced, meta.originSet, request.set)
    when (request.prio(0)) {
      final_meta_writeback.source := request.source
    }
  }

  when (bad_grant) {
    when (meta.hit) {
      // upgrade failed (B -> T)
      assert (!meta_valid || meta.state === BRANCH)
      final_meta_writeback.hit     := true.B
      final_meta_writeback.dirty   := false.B
      final_meta_writeback.state   := BRANCH
      final_meta_writeback.clients := meta.clients & ~probes_toN
    } .otherwise {
      // failed N -> (T or B)
      final_meta_writeback.hit     := false.B
      final_meta_writeback.dirty   := false.B
      final_meta_writeback.state   := INVALID
      final_meta_writeback.clients := 0.U
      final_meta_writeback.source  := 0.U
    }
  }

  val invalid = Wire(new DirectoryEntry(params))
  invalid.dirty   := false.B
  invalid.state   := INVALID
  invalid.clients := 0.U
  invalid.tag     := 0.U
  invalid.displaced := false.B  // SSBC: invalid entries are not displaced
  invalid.originSet := 0.U      // SSBC: no origin for invalid entries
  invalid.source  := 0.U

  // Just because a client says BtoT, by the time we process the request he may be N.
  // Therefore, we must consult our own meta-data state to confirm he owns the line still.
  val honour_BtoT = meta.hit && (meta.clients & req_clientBit).orR

  // The client asking us to act is proof they don't have permissions.
  val excluded_client = Mux(meta.hit && request.prio(0) && skipProbeN(request.opcode, params.cache.hintsSkipProbe), req_clientBit, 0.U)
  val evict_excluded = Mux(migrate_evict_partner, 0.U, excluded_client)
  io.schedule.bits.a.bits.tag     := request.tag
  io.schedule.bits.a.bits.set     := request.set
  io.schedule.bits.a.bits.param   := Mux(req_needT, Mux(meta.hit, BtoT, NtoT), NtoB)
  io.schedule.bits.a.bits.block   := request.size =/= log2Ceil(params.cache.blockBytes).U ||
                                     !(request.opcode === PutFullData || request.opcode === AcquirePerm)
  io.schedule.bits.a.bits.source  := 0.U
  io.schedule.bits.b.bits.param   := Mux(!s_rprobe, toN, Mux(request.prio(1), request.param, Mux(req_needT, toN, toB)))
  io.schedule.bits.b.bits.tag     := Mux(!s_rprobe, evictTag, request.tag)
  io.schedule.bits.b.bits.set     := evictSet
  io.schedule.bits.b.bits.clients := evictClients & ~evict_excluded
  io.schedule.bits.c.bits.opcode  := Mux(evictDirty, ReleaseData, Release)
  io.schedule.bits.c.bits.param   := Mux(evictState === BRANCH, BtoN, TtoN)
  io.schedule.bits.c.bits.source  := 0.U
  io.schedule.bits.c.bits.tag     := evictTag
  io.schedule.bits.c.bits.set     := evictSet     // logical set for TileLink address
  io.schedule.bits.c.bits.physSet := evictPhysSet // physical set for BankedStore SRAM read
  io.schedule.bits.c.bits.way     := evictWay
  io.schedule.bits.c.bits.dirty   := evictDirty
  io.schedule.bits.d.bits.viewAsSupertype(chiselTypeOf(request)) := request
  io.schedule.bits.d.bits.set     := meta.set
  io.schedule.bits.d.bits.param   := Mux(!req_acquire, request.param,
                                       MuxCase(request.param, Seq(
                                         (request.param === NtoB) -> Mux(req_promoteT, NtoT, NtoB),
                                         (request.param === BtoT) -> Mux(honour_BtoT,  BtoT, NtoT),
                                         (request.param === NtoT) -> NtoT)))
  io.schedule.bits.d.bits.sink    := 0.U
  io.schedule.bits.d.bits.way     := meta.way
  io.schedule.bits.d.bits.bad     := bad_grant
  io.schedule.bits.e.bits.sink    := sink
  io.schedule.bits.x.bits.fail    := false.B
  io.schedule.bits.dir.bits.set   := evictPhysSet  // physical set: partner set or meta.set
  io.schedule.bits.dir.bits.way   := evictWay
  // dir.bits.data selection:
  //   !s_release (evict DIR write)    → invalid  (clearing the old occupant)
  //   !s_dir_evict (deferred evict)   → invalid  (dirty eviction slot cleared after ReleaseAck)
  //   !s_writeback (writeback phase)  → final_meta_writeback  (installing the new line)
  val dirWriteIsEvict = !s_release || (!s_dir_evict && w_releaseack && s_release)
  io.schedule.bits.dir.bits.data  := Mux(dirWriteIsEvict, invalid, WireInit(new DirectoryEntry(params), init = final_meta_writeback))

  // Coverage of state transitions
  def cacheState(entry: DirectoryEntry, hit: Bool) = {
    val out = WireDefault(0.U)
    val c = entry.clients.orR
    val d = entry.dirty
    switch (entry.state) {
      is (BRANCH)  { out := Mux(c, S_BRANCH_C.code, S_BRANCH.code) }
      is (TRUNK)   { out := Mux(d, S_TRUNK_CD.code, S_TRUNK_C.code) }
      is (TIP)     { out := Mux(c, Mux(d, S_TIP_CD.code, S_TIP_C.code), Mux(d, S_TIP_D.code, S_TIP.code)) }
      is (INVALID) { out := S_INVALID.code }
    }
    when (!hit) { out := S_INVALID.code }
    out
  }

  val p = !params.lastLevel  // can be probed
  val c = !params.firstLevel // can be acquired
  val m = params.inner.client.clients.exists(!_.supports.probe)   // can be written (or read)
  val r = params.outer.manager.managers.exists(!_.alwaysGrantsT) // read-only devices exist
  val f = params.control     // flush control register exists
  val cfg = (p, c, m, r, f)
  val b = r || p // can reach branch state (via probe downgrade or read-only device)

  // The cache must be used for something or we would not be here
  require(c || m)

  val evict = cacheState(meta, !meta.hit)
  val before = cacheState(meta, meta.hit)
  val after  = cacheState(final_meta_writeback, true.B)

  def eviction(from: CacheState, cover: Boolean): Unit = {
    if (cover) {
      params.ccover(evict === from.code, s"MSHR_${from}_EVICT", s"State transition from ${from} to evicted ${cfg}")
    } else {
      assert(!(evict === from.code), cf"State transition from ${from} to evicted should be impossible ${cfg}")
    }
    if (cover && f) {
      params.ccover(before === from.code, s"MSHR_${from}_FLUSH", s"State transition from ${from} to flushed ${cfg}")
    } else {
      assert(!(before === from.code), cf"State transition from ${from} to flushed should be impossible ${cfg}")
    }
  }

  def transition(from: CacheState, to: CacheState, cover: Boolean): Unit = {
    if (cover) {
      params.ccover(before === from.code && after === to.code, s"MSHR_${from}_${to}", s"State transition from ${from} to ${to} ${cfg}")
    } else {
      assert(!(before === from.code && after === to.code), cf"State transition from ${from} to ${to} should be impossible ${cfg}")
    }
  }

  when ((!s_release && w_rprobeackfirst) && io.schedule.ready) {
    eviction(S_BRANCH,    b)      // MMIO read to read-only device
    eviction(S_BRANCH_C,  b && c) // you need children to become C
    eviction(S_TIP,       true)   // MMIO read || clean release can lead to this state
    eviction(S_TIP_C,     c)      // needs two clients || client + mmio || downgrading client
    eviction(S_TIP_CD,    c)      // needs two clients || client + mmio || downgrading client
    eviction(S_TIP_D,     true)   // MMIO write || dirty release lead here
    eviction(S_TRUNK_C,   c)      // acquire for write
    eviction(S_TRUNK_CD,  c)      // dirty release then reacquire
  }

  when ((!s_writeback && no_wait) && io.schedule.ready) {
    transition(S_INVALID,  S_BRANCH,   b && m) // only MMIO can bring us to BRANCH state
    transition(S_INVALID,  S_BRANCH_C, b && c) // C state is only possible if there are inner caches
    transition(S_INVALID,  S_TIP,      m)      // MMIO read
    transition(S_INVALID,  S_TIP_C,    false)  // we would go S_TRUNK_C instead
    transition(S_INVALID,  S_TIP_CD,   false)  // acquire does not cause dirty immediately
    transition(S_INVALID,  S_TIP_D,    m)      // MMIO write
    transition(S_INVALID,  S_TRUNK_C,  c)      // acquire
    transition(S_INVALID,  S_TRUNK_CD, false)  // acquire does not cause dirty immediately

    transition(S_BRANCH,   S_INVALID,  b && p) // probe can do this (flushes run as evictions)
    transition(S_BRANCH,   S_BRANCH_C, b && c) // acquire
    transition(S_BRANCH,   S_TIP,      b && m) // prefetch write
    transition(S_BRANCH,   S_TIP_C,    false)  // we would go S_TRUNK_C instead
    transition(S_BRANCH,   S_TIP_CD,   false)  // acquire does not cause dirty immediately
    transition(S_BRANCH,   S_TIP_D,    b && m) // MMIO write
    transition(S_BRANCH,   S_TRUNK_C,  b && c) // acquire
    transition(S_BRANCH,   S_TRUNK_CD, false)  // acquire does not cause dirty immediately

    transition(S_BRANCH_C, S_INVALID,  b && c && p)
    transition(S_BRANCH_C, S_BRANCH,   b && c)      // clean release (optional)
    transition(S_BRANCH_C, S_TIP,      b && c && m) // prefetch write
    transition(S_BRANCH_C, S_TIP_C,    false)       // we would go S_TRUNK_C instead
    transition(S_BRANCH_C, S_TIP_D,    b && c && m) // MMIO write
    transition(S_BRANCH_C, S_TIP_CD,   false)       // going dirty means we must shoot down clients
    transition(S_BRANCH_C, S_TRUNK_C,  b && c)      // acquire
    transition(S_BRANCH_C, S_TRUNK_CD, false)       // acquire does not cause dirty immediately

    transition(S_TIP,      S_INVALID,  p)
    transition(S_TIP,      S_BRANCH,   p)      // losing TIP only possible via probe
    transition(S_TIP,      S_BRANCH_C, false)  // we would go S_TRUNK_C instead
    transition(S_TIP,      S_TIP_C,    false)  // we would go S_TRUNK_C instead
    transition(S_TIP,      S_TIP_D,    m)      // direct dirty only via MMIO write
    transition(S_TIP,      S_TIP_CD,   false)  // acquire does not make us dirty immediately
    transition(S_TIP,      S_TRUNK_C,  c)      // acquire
    transition(S_TIP,      S_TRUNK_CD, false)  // acquire does not make us dirty immediately

    transition(S_TIP_C,    S_INVALID,  c && p)
    transition(S_TIP_C,    S_BRANCH,   c && p) // losing TIP only possible via probe
    transition(S_TIP_C,    S_BRANCH_C, c && p) // losing TIP only possible via probe
    transition(S_TIP_C,    S_TIP,      c)      // probed while MMIO read || clean release (optional)
    transition(S_TIP_C,    S_TIP_D,    c && m) // direct dirty only via MMIO write
    transition(S_TIP_C,    S_TIP_CD,   false)  // going dirty means we must shoot down clients
    transition(S_TIP_C,    S_TRUNK_C,  c)      // acquire
    transition(S_TIP_C,    S_TRUNK_CD, false)  // acquire does not make us immediately dirty

    transition(S_TIP_D,    S_INVALID,  p)
    transition(S_TIP_D,    S_BRANCH,   p)      // losing D is only possible via probe
    transition(S_TIP_D,    S_BRANCH_C, p && c) // probed while acquire shared
    transition(S_TIP_D,    S_TIP,      p)      // probed while MMIO read || outer probe.toT (optional)
    transition(S_TIP_D,    S_TIP_C,    false)  // we would go S_TRUNK_C instead
    transition(S_TIP_D,    S_TIP_CD,   false)  // we would go S_TRUNK_CD instead
    transition(S_TIP_D,    S_TRUNK_C,  p && c) // probed while acquired
    transition(S_TIP_D,    S_TRUNK_CD, c)      // acquire

    transition(S_TIP_CD,   S_INVALID,  c && p)
    transition(S_TIP_CD,   S_BRANCH,   c && p) // losing D is only possible via probe
    transition(S_TIP_CD,   S_BRANCH_C, c && p) // losing D is only possible via probe
    transition(S_TIP_CD,   S_TIP,      c && p) // probed while MMIO read || outer probe.toT (optional)
    transition(S_TIP_CD,   S_TIP_C,    false)  // we would go S_TRUNK_C instead
    transition(S_TIP_CD,   S_TIP_D,    c)      // MMIO write || clean release (optional)
    transition(S_TIP_CD,   S_TRUNK_C,  c && p) // probed while acquire
    transition(S_TIP_CD,   S_TRUNK_CD, c)      // acquire

    transition(S_TRUNK_C,  S_INVALID,  c && p)
    transition(S_TRUNK_C,  S_BRANCH,   c && p) // losing TIP only possible via probe
    transition(S_TRUNK_C,  S_BRANCH_C, c && p) // losing TIP only possible via probe
    transition(S_TRUNK_C,  S_TIP,      c)      // MMIO read || clean release (optional)
    transition(S_TRUNK_C,  S_TIP_C,    c)      // bounce shared
    transition(S_TRUNK_C,  S_TIP_D,    c)      // dirty release
    transition(S_TRUNK_C,  S_TIP_CD,   c)      // dirty bounce shared
    transition(S_TRUNK_C,  S_TRUNK_CD, c)      // dirty bounce

    transition(S_TRUNK_CD, S_INVALID,  c && p)
    transition(S_TRUNK_CD, S_BRANCH,   c && p) // losing D only possible via probe
    transition(S_TRUNK_CD, S_BRANCH_C, c && p) // losing D only possible via probe
    transition(S_TRUNK_CD, S_TIP,      c && p) // probed while MMIO read || outer probe.toT (optional)
    transition(S_TRUNK_CD, S_TIP_C,    false)  // we would go S_TRUNK_C instead
    transition(S_TRUNK_CD, S_TIP_D,    c)      // dirty release
    transition(S_TRUNK_CD, S_TIP_CD,   c)      // bounce shared
    transition(S_TRUNK_CD, S_TRUNK_C,  c && p) // probed while acquire
  }

  // Handle response messages
  val probe_bit = params.clientBit(io.sinkc.bits.source)
  val probe_target_clients = evictClients & ~evict_excluded
  val last_probe = (probes_done | probe_bit) === probe_target_clients
  val probe_toN = isToN(io.sinkc.bits.param)
  if (!params.firstLevel) when (io.sinkc.valid) {
    when (migrate_evict_partner) {
      printf("[InclusiveCache][SSBC MSHR %d] PARTNER_PROBE_ACK src=%d set=%d tag=0x%x done=0x%x target=0x%x last=%d\n",
             id.U,
             io.sinkc.bits.source, io.sinkc.bits.set, io.sinkc.bits.tag,
             probes_done | probe_bit, probe_target_clients, last_probe)
    }
    params.ccover( probe_toN && io.schedule.bits.b.bits.param === toB, "MSHR_PROBE_FULL", "Client downgraded to N when asked only to do B")
    params.ccover(!probe_toN && io.schedule.bits.b.bits.param === toB, "MSHR_PROBE_HALF", "Client downgraded to B when asked only to do B")
    // Caution: the probe matches us only in set.
    // We would never allow an outer probe to nest until both w_[rp]probeack complete, so
    // it is safe to just unguardedly update the probe FSM.
    probes_done := probes_done | probe_bit
    probes_toN := probes_toN | Mux(probe_toN, probe_bit, 0.U)
    probes_noT := probes_noT || io.sinkc.bits.param =/= TtoT
    val probe_last_beat = io.sinkc.bits.last || !io.sinkc.bits.data
    w_rprobeackfirst := w_rprobeackfirst || last_probe
    w_rprobeacklast := w_rprobeacklast || (last_probe && probe_last_beat)
    w_pprobeackfirst := w_pprobeackfirst || last_probe
    w_pprobeacklast := w_pprobeacklast || (last_probe && probe_last_beat)
    // Allow wormhole routing from sinkC if the first request beat has offset 0
    val set_pprobeack = last_probe && (io.sinkc.bits.last || request.offset === 0.U)
    w_pprobeack := w_pprobeack || set_pprobeack
    params.ccover(!set_pprobeack && w_rprobeackfirst, "MSHR_PROBE_SERIAL", "Sequential routing of probe response data")
    params.ccover( set_pprobeack && w_rprobeackfirst, "MSHR_PROBE_WORMHOLE", "Wormhole routing of probe response data")
    // However, meta-data updates need to be done more cautiously
    when (meta.state =/= INVALID && io.sinkc.bits.tag === meta.tag && io.sinkc.bits.data) { meta.dirty := true.B } // !!!
  }
  when (io.sinkd.valid) {
    when (io.sinkd.bits.opcode === Grant || io.sinkd.bits.opcode === GrantData) {
      sink := io.sinkd.bits.sink
      w_grantfirst := true.B
      w_grantlast := io.sinkd.bits.last
      // Record if we need to prevent taking ownership
      bad_grant := io.sinkd.bits.denied
      // Allow wormhole routing for requests whose first beat has offset 0
      w_grant := request.offset === 0.U || io.sinkd.bits.last
      params.ccover(io.sinkd.bits.opcode === GrantData && request.offset === 0.U, "MSHR_GRANT_WORMHOLE", "Wormhole routing of grant response data")
      params.ccover(io.sinkd.bits.opcode === GrantData && request.offset =/= 0.U, "MSHR_GRANT_SERIAL", "Sequential routing of grant response data")
      gotT := io.sinkd.bits.param === toT
    }
    .elsewhen (io.sinkd.bits.opcode === ReleaseAck) {
      w_releaseack := true.B
    }
  }
  when (io.sinke.valid) {
    w_grantack := true.B
  }

  // Bootstrap new requests
  val allocate_as_full = WireInit(new FullRequest(params), init = io.allocate.bits)
  val new_meta = Mux(io.allocate.valid && io.allocate.bits.repeat, final_meta_writeback, dir_final)
  val new_request = Mux(io.allocate.valid, allocate_as_full, request)
  val new_needT = needT(new_request.opcode, new_request.param)
  val new_clientBit = params.clientBit(new_request.source)
  val new_skipProbe = Mux(skipProbeN(new_request.opcode, params.cache.hintsSkipProbe), new_clientBit, 0.U)

  val prior = cacheState(final_meta_writeback, true.B)
  def bypass(from: CacheState, cover: Boolean): Unit = {
    if (cover) {
      params.ccover(prior === from.code, s"MSHR_${from}_BYPASS", s"State bypass transition from ${from} ${cfg}")
    } else {
      assert(!(prior === from.code), cf"State bypass from ${from} should be impossible ${cfg}")
    }
  }

  when (io.allocate.valid && io.allocate.bits.repeat) {
    bypass(S_INVALID,   f || p) // Can lose permissions (probe/flush)
    bypass(S_BRANCH,    b)      // MMIO read to read-only device
    bypass(S_BRANCH_C,  b && c) // you need children to become C
    bypass(S_TIP,       true)   // MMIO read || clean release can lead to this state
    bypass(S_TIP_C,     c)      // needs two clients || client + mmio || downgrading client
    bypass(S_TIP_CD,    c)      // needs two clients || client + mmio || downgrading client
    bypass(S_TIP_D,     true)   // MMIO write || dirty release lead here
    bypass(S_TRUNK_C,   c)      // acquire for write
    bypass(S_TRUNK_CD,  c)      // dirty release then reacquire
  }

  when (io.allocate.valid) {
    assert (!request_valid || (no_wait && io.schedule.fire))
    request_valid := true.B
    request := io.allocate.bits
  }

  // Create execution plan
  when (dir_final_valid || (io.allocate.valid && io.allocate.bits.repeat)) {
    meta_valid := true.B
    meta := new_meta
    probes_done := 0.U
    probes_toN := 0.U
    probes_noT := false.B
    gotT := false.B
    bad_grant := false.B

    // These should already be either true or turning true
    // We clear them here explicitly to simplify the mux tree
    s_rprobe         := true.B
    w_rprobeackfirst := true.B
    w_rprobeacklast  := true.B
    s_release        := true.B
    w_releaseack     := true.B
    s_pprobe         := true.B
    s_acquire        := true.B
    s_flush          := true.B
    s_dir_evict      := true.B  // no pending deferred dir-evict write
    w_grantfirst     := true.B
    w_grantlast      := true.B
    w_grant          := true.B
    w_pprobeackfirst := true.B
    w_pprobeacklast  := true.B
    w_pprobeack      := true.B
    s_probeack       := true.B
    s_grantack       := true.B
    s_execute        := true.B
    w_grantack       := true.B
    s_writeback      := true.B
    
    // Migration states - reset
    s_migrate_lookup := true.B
    w_migrate_lookup := true.B
    s_migrate        := true.B
    w_migrate_done   := true.B
    migrate_valid    := false.B
    
    when (dir_final_valid && shouldMigrate && !srcCheap) {
      printf("[InclusiveCache][SSBC MSHR %d] MIGRATE_SKIP_SRC srcSet=%d srcTag=0x%x srcClients=0x%x (srcCheap=0 -> normal evict)\n",
             id.U, dir_final.set, dir_final.tag, dir_final.clients)
    }

    // Capture migration info from directory result
    when (dir_final_valid && doMigrate) {
      migrate_valid := true.B
      migrate_partnerSet := dir_final.partnerSet
      migrate_partnerWay := dir_final.partnerWay
      printf("[InclusiveCache][SSBC MSHR %d] MIGRATE_TRIGGER srcSet=%d srcTag=0x%x -> partnerSet=%d partnerWay=%d\n",
             id.U, dir_final.set, dir_final.tag, 
             dir_final.partnerSet, dir_final.partnerWay)
    }
    // DEBUG: show what doMigrate evaluated to when dir_final_valid fires
    when (dir_final_valid) {
      printf("[InclusiveCache][SSBC MSHR %d] DIR_FINAL_VALID hit=%d state=%d clients=0x%x srcCheap=%d doMigrate=%d shouldMigrate=%d displaced=%d currentSat=%d partnerSat=%d\n",
             id.U, dir_final.hit, dir_final.state, dir_final.clients, srcCheap, doMigrate, shouldMigrate,
             dir_final.displaced, dir_final.currentSat, dir_final.partnerSat)
    }

    // For C channel requests (ie: Release[Data])
    when (new_request.prio(2) && (!params.firstLevel).B) {
      s_execute := false.B
      // Do we need to go dirty?
      when (new_request.opcode(0) && !new_meta.dirty) {
        s_writeback := false.B
      }
      // Does our state change?
      when (isToB(new_request.param) && new_meta.state === TRUNK) {
        s_writeback := false.B
      }
      // Do our clients change?
      when (isToN(new_request.param) && (new_meta.clients & new_clientBit) =/= 0.U) {
        s_writeback := false.B
      }
      assert (new_meta.hit)
    }
    // For X channel requests (ie: flush or invalidate)
    .elsewhen (new_request.control.flush && params.control.B) { // new_request.prio(0)
      s_flush := false.B
      // Do we need to actually do something?
      when (new_meta.hit) {
        s_release := false.B
        // If flush request is an invalidate, don't writeback cache block. Just clear directory
        w_releaseack := Mux(new_request.control.invalidate, true.B, false.B)
        // Do we need to shoot-down inner caches?
        when ((!params.firstLevel).B && (new_meta.clients =/= 0.U)) {
          s_rprobe := false.B
          w_rprobeackfirst := false.B
          w_rprobeacklast := false.B
        }
      }
    }
    // For A channel requests
    .otherwise { // new_request.prio(0) && !new_request.control
      s_execute := false.B
      // Do we need an eviction?
      when (!new_meta.hit && new_meta.state =/= INVALID) {
        // Check if migration is enabled for this eviction
        when (dir_final_valid && doMigrate) {
          // Migration path: lookup partner set, then migrate instead of release to memory
          s_migrate_lookup := false.B
          w_migrate_lookup := false.B
          s_migrate := false.B
          w_migrate_done := false.B
          s_release := true.B  // Skip normal release - migration will handle it
          w_releaseack := true.B
        } .otherwise {
          // Normal eviction path: release to memory
          s_release := false.B
          w_releaseack := false.B
          // For dirty lines the DIR INVALID write must wait for w_releaseack.
          // For clean lines w_releaseack starts true so s_dir_evict can stay true
          // (the dir write fires immediately as before with no behaviour change).
          when (new_meta.dirty) { s_dir_evict := false.B }
        }
        // Do we need to shoot-down inner caches?
        when ((!params.firstLevel).B & (new_meta.clients =/= 0.U)) {
          s_rprobe := false.B
          w_rprobeackfirst := false.B
          w_rprobeacklast := false.B
        }
      }
      // Do we need an acquire?
      when (!new_meta.hit || (new_meta.state === BRANCH && new_needT)) {
        s_acquire := false.B
        w_grantfirst := false.B
        w_grantlast := false.B
        w_grant := false.B
        s_grantack := false.B
        s_writeback := false.B
      }
      // Do we need a probe?
      when ((!params.firstLevel).B && (new_meta.hit &&
            (new_needT || new_meta.state === TRUNK) &&
            (new_meta.clients & ~new_skipProbe) =/= 0.U)) {
        s_pprobe := false.B
        w_pprobeackfirst := false.B
        w_pprobeacklast := false.B
        w_pprobeack := false.B
        s_writeback := false.B
      }
      // Do we need a grantack?
      when (new_request.opcode === AcquireBlock || new_request.opcode === AcquirePerm) {
        w_grantack := false.B
        s_writeback := false.B
      }
      // Becomes dirty?
      when (!new_request.opcode(2) && new_meta.hit && !new_meta.dirty) {
        s_writeback := false.B
      }
    }
  }
}
