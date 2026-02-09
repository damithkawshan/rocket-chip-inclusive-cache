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
import org.chipsalliance.cde.config._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.tilelink._
import MetaData._
import chisel3.experimental.dataview._
import freechips.rocketchip.util.DescribedSRAM

class DirectoryEntry(params: InclusiveCacheParameters) extends InclusiveCacheBundle(params)
{
  val dirty   = Bool() // true => TRUNK or TIP
  val state   = UInt(params.stateBits.W)
  val clients = UInt(params.clientBits.W)
  val tag     = UInt(params.tagBits.W)
  // SSBC: displaced bit - true if line was displaced here from its partner set
  val displaced = Bool()  // d=0: native to this set, d=1: displaced from partner set
  // SSBC: origin set (logical home set for a displaced line)
  val originSet = UInt(params.setBits.W)
  // Source ID of the last writer (TileLink A-channel source)
  val source  = UInt(params.inner.bundle.sourceBits.W)
}

class DirectoryWrite(params: InclusiveCacheParameters) extends InclusiveCacheBundle(params)
{
  val set  = UInt(params.setBits.W)
  val way  = UInt(params.wayBits.W)
  val data = new DirectoryEntry(params)
}

class DirectoryRead(params: InclusiveCacheParameters) extends InclusiveCacheBundle(params)
{
  val set = UInt(params.setBits.W)
  val tag = UInt(params.tagBits.W)
  val source = UInt(params.inner.bundle.sourceBits.W)
}

// SSBC saturation counter update (issued by controller after final hit/miss)
class SatCounterUpdate(params: InclusiveCacheParameters) extends InclusiveCacheBundle(params)
{
  val set = UInt(params.setBits.W)
  val inc = Bool()
  val dec = Bool()
}

class DirectoryResult(params: InclusiveCacheParameters) extends DirectoryEntry(params)
{
  val hit = Bool()
  val way = UInt(params.wayBits.W)
  val set = UInt(params.setBits.W)
  // SSBC visibility for controller
  val scBit = Bool()
  val partnerScBit = Bool()
  val currentSat = UInt(log2Ceil(2 * params.cache.ways).W)
  val partnerSat = UInt(log2Ceil(2 * params.cache.ways).W)
  val partnerSet = UInt(params.setBits.W)
  val partnerWay = UInt(params.wayBits.W)  // LRU victim way in partner set for migration
  // SSBC: secondary search control/summary
  val secondaryHit = Bool()
  val secondaryWay = UInt(params.wayBits.W)
}

// Request to lookup partner set for migration
class PartnerLookupRequest(params: InclusiveCacheParameters) extends InclusiveCacheBundle(params)
{
  val set = UInt(params.setBits.W)  // The partner set to lookup
}

// Result of partner set lookup - provides victim way for migration
class PartnerLookupResult(params: InclusiveCacheParameters) extends InclusiveCacheBundle(params)
{
  val way = UInt(params.wayBits.W)
  val victimTag = UInt(params.tagBits.W)
  val victimDirty = Bool()
  val victimValid = Bool()
  val victimClients = UInt(params.clientBits.W)
  val victimState = UInt(params.stateBits.W)
}

class Directory(params: InclusiveCacheParameters) extends Module
{
  val ssbcEnabled = params.cache.ssbcEnabled.B

  val io = IO(new Bundle {
    val write  = Flipped(Decoupled(new DirectoryWrite(params)))
    val read   = Flipped(Valid(new DirectoryRead(params))) // sees same-cycle write
    val result = Valid(new DirectoryResult(params))
    val ready  = Bool() // reset complete; can enable access
    // Saturation counter update from controller (final hit/miss)
    val satUpdate = Flipped(Valid(new SatCounterUpdate(params)))
    // Saturation counters output (one per set)
    val satCounters = Output(Vec(params.cache.sets, UInt(log2Ceil(2 * params.cache.ways).W)))
    // Second search bits output (one per set) - SSBC algorithm
    val secondSearch = Output(Vec(params.cache.sets, Bool()))
    // Partner set lookup for migration
    val partnerLookup = Flipped(Valid(new PartnerLookupRequest(params)))
    val partnerResult = Valid(new PartnerLookupResult(params))
  })

  // SSBC Saturation counter parameters
  // Range: 0 to 2K-1 where K is associativity (number of ways)
  val K = params.cache.ways
  val satCounterMax = (2 * K - 1).U
  val satCounterWidth = log2Ceil(2 * K)
  // Displacement thresholds per SSBC paper:
  // - Source set eligible to displace only if counter at maximum (2K-1)
  // - Destination set is good receiver if counter < K
  val satCounterHighThreshold = satCounterMax  // Must be at max to displace
  val satCounterLowThreshold = K.U             // Must be below K to receive
  
  // Saturation counters - one per set, range [0, 2K-1]
  val satCounters = RegInit(VecInit(Seq.fill(params.cache.sets)(0.U(satCounterWidth.W))))
  io.satCounters := satCounters
  
  // SSBC Second search bits - one per set
  // sc[i] = 1 means: the partner set of i may hold displaced lines belonging to set i
  val secondSearchBits = RegInit(VecInit(Seq.fill(params.cache.sets)(false.B)))
  io.secondSearch := secondSearchBits

  val codeBits = new DirectoryEntry(params).getWidth

  val cc_dir =  DescribedSRAM(
    name = "cc_dir",
    desc = "Directory RAM",
    size = params.cache.sets,
    data = Vec(params.cache.ways, UInt(codeBits.W))
  )

  val write = Queue(io.write, 1) // must inspect contents => max size 1
  // a flow Q creates a WaR hazard... this MIGHT not cause a problem
  // a pipe Q causes combinational loop through the scheduler

  // Wiping the Directory with 0s on reset has ultimate priority
  val wipeCount = RegInit(0.U((params.setBits + 1).W))
  val wipeOff = RegNext(false.B, true.B) // don't wipe tags during reset
  val wipeDone = wipeCount(params.setBits)
  val wipeSet = wipeCount(params.setBits - 1,0)

  io.ready := wipeDone
  when (!wipeDone && !wipeOff) { wipeCount := wipeCount + 1.U }
  assert (wipeDone || !io.read.valid)

  // Be explicit for dumb 1-port inference
  val ren = io.read.valid
  val wen = (!wipeDone && !wipeOff) || write.valid
  assert (!io.read.valid || wipeDone)

  require (codeBits <= 256)

  write.ready := !io.read.valid
  when (!ren && wen) {
    cc_dir.write(
      Mux(wipeDone, write.bits.set, wipeSet),
      VecInit.fill(params.cache.ways) { Mux(wipeDone, write.bits.data.asUInt, 0.U) },
      UIntToOH(write.bits.way, params.cache.ways).asBools.map(_ || !wipeDone))
  }

  val ren1 = RegInit(false.B)
  val ren2 = if (params.micro.dirReg) RegInit(false.B) else ren1
  ren2 := ren1
  ren1 := ren

  val bypass_valid = params.dirReg(write.valid)
  val bypass = params.dirReg(write.bits, ren1 && write.valid)
  val regout = params.dirReg(cc_dir.read(io.read.bits.set, ren), ren1)
  val tag = params.dirReg(RegEnable(io.read.bits.tag, ren), ren1)
  val set = params.dirReg(RegEnable(io.read.bits.set, ren), ren1)
  val reqSource = params.dirReg(RegEnable(io.read.bits.source, ren), ren1)

  // Compute the victim way in case of an evicition
  val victimLFSR = random.LFSR(width = 16, params.dirReg(ren))(InclusiveCacheParameters.lfsrBits-1, 0)
  val victimSums = Seq.tabulate(params.cache.ways) { i => ((1 << InclusiveCacheParameters.lfsrBits)*i / params.cache.ways).U }
  val victimLTE  = Cat(victimSums.map { _ <= victimLFSR }.reverse)
  val victimSimp = Cat(0.U(1.W), victimLTE(params.cache.ways-1, 1), 1.U(1.W))
  val victimWayOH = victimSimp(params.cache.ways-1,0) & ~(victimSimp >> 1)
  val victimWay = OHToUInt(victimWayOH)
  assert (!ren2 || victimLTE(0) === 1.U)
  assert (!ren2 || ((victimSimp >> 1) & ~victimSimp) === 0.U) // monotone
  assert (!ren2 || PopCount(victimWayOH) === 1.U)

  val setQuash = bypass_valid && bypass.set === set
  val tagMatch = bypass.data.tag === tag
  val wayMatch = bypass.way === victimWay

  val ways = regout.map(d => d.asTypeOf(new DirectoryEntry(params)))
  val hits = Cat(ways.zipWithIndex.map { case (w, i) =>
    w.tag === tag && w.state =/= INVALID && (!setQuash || i.U =/= bypass.way)
  }.reverse)
  val hit = hits.orR
  // Use original hit logic for functional behavior (match upstream);
  // bypassed same-cycle writes are not treated as hits here.
  val primaryHit = hit || (setQuash && tagMatch && bypass.data.state =/= INVALID)
  val primaryMiss = !primaryHit
  
  // Calculate partner set by inverting MSB
  // Ex: Set 000010 partner: 100010, Set 001010 partner: 101010
  val partnerSet = Cat(~set(params.setBits-1), set(params.setBits-2, 0))
  
  // Get saturation counters for current set and partner set
  val currentSatCounter = satCounters(set)
  val partnerSatCounter = satCounters(partnerSet)
  
  // Determine if evicting line should migrate to partner set (SSBC algorithm)
  // Conditions per SSBC paper:
  //   1) This is a miss (eviction will happen)
  //   2) Current set is at maximum saturation (counter == 2K-1)
  //   3) Partner set is underutilized (counter < K)
  //   4) The victim way has valid data to migrate
  val hitEntry = Mux(setQuash && tagMatch, bypass.data, Mux1H(hits, ways))
  val victimEntry = Mux(setQuash && (tagMatch || wayMatch), bypass.data, Mux1H(victimWayOH, ways))
  val victimValid = victimEntry.state =/= INVALID  // Victim must be valid to migrate
  val resultEntry = Mux(hit, hitEntry, victimEntry)

  io.result.valid := ren2
  io.result.bits.viewAsSupertype(chiselTypeOf(bypass.data)) := resultEntry
  io.result.bits.hit := hit || (setQuash && tagMatch && bypass.data.state =/= INVALID)
  io.result.bits.way := Mux(hit, OHToUInt(hits), Mux(setQuash && tagMatch, bypass.way, victimWay))
  io.result.bits.set := set
  io.result.bits.scBit := secondSearchBits(set)
  io.result.bits.partnerScBit := secondSearchBits(partnerSet)
  io.result.bits.currentSat := currentSatCounter
  io.result.bits.partnerSat := partnerSatCounter
  io.result.bits.partnerSet := partnerSet
  io.result.bits.partnerWay := victimWay  // Use same random way selection for partner
  io.result.bits.secondaryHit := false.B
  io.result.bits.secondaryWay := 0.U
  
  // Partner set lookup logic for migration
  // This uses a separate read path to lookup the partner set's victim
  val pren = io.partnerLookup.valid && wipeDone && !ren  // Only when not doing normal read
  val pren1 = RegInit(false.B)
  val pren2 = if (params.micro.dirReg) RegInit(false.B) else pren1
  pren2 := pren1
  pren1 := pren
  
  val partnerLookupSet = RegEnable(io.partnerLookup.bits.set, pren)
  val partnerRegout = params.dirReg(cc_dir.read(io.partnerLookup.bits.set, pren), pren1)
  
  // Compute victim way for partner set using same LFSR-based selection
  val partnerVictimLFSR = random.LFSR(width = 16, params.dirReg(pren))(InclusiveCacheParameters.lfsrBits-1, 0)
  val partnerVictimSums = Seq.tabulate(params.cache.ways) { i => ((1 << InclusiveCacheParameters.lfsrBits)*i / params.cache.ways).U }
  val partnerVictimLTE = Cat(partnerVictimSums.map { _ <= partnerVictimLFSR }.reverse)
  val partnerVictimSimp = Cat(0.U(1.W), partnerVictimLTE(params.cache.ways-1, 1), 1.U(1.W))
  val partnerVictimWayOH = partnerVictimSimp(params.cache.ways-1,0) & ~(partnerVictimSimp >> 1)
  val partnerVictimWay = OHToUInt(partnerVictimWayOH)
  
  val partnerWays = partnerRegout.map(d => d.asTypeOf(new DirectoryEntry(params)))
  val partnerVictimEntry = Mux1H(partnerVictimWayOH, partnerWays)
  
  // Handle bypass for partner lookup
  val partnerBypassValid = params.dirReg(write.valid)
  val partnerBypass = params.dirReg(write.bits, pren1 && write.valid)
  val partnerSetQuash = partnerBypassValid && partnerBypass.set === partnerLookupSet
  val partnerWayMatch = partnerBypass.way === partnerVictimWay
  
  io.partnerResult.valid := pren2
  io.partnerResult.bits.way := partnerVictimWay
  io.partnerResult.bits.victimTag := Mux(partnerSetQuash && partnerWayMatch, partnerBypass.data.tag, partnerVictimEntry.tag)
  io.partnerResult.bits.victimDirty := Mux(partnerSetQuash && partnerWayMatch, partnerBypass.data.dirty, partnerVictimEntry.dirty)
  io.partnerResult.bits.victimValid := Mux(partnerSetQuash && partnerWayMatch, partnerBypass.data.state =/= INVALID, partnerVictimEntry.state =/= INVALID)
  io.partnerResult.bits.victimClients := Mux(partnerSetQuash && partnerWayMatch, partnerBypass.data.clients, partnerVictimEntry.clients)
  io.partnerResult.bits.victimState := Mux(partnerSetQuash && partnerWayMatch, partnerBypass.data.state, partnerVictimEntry.state)
  
  // Update saturation counters from controller after final hit/miss
  when (io.satUpdate.valid && ssbcEnabled) {
    assert(!(io.satUpdate.bits.inc && io.satUpdate.bits.dec))
    val updSet = io.satUpdate.bits.set
    val cnt = satCounters(updSet)
    when (io.satUpdate.bits.dec) {
      when (cnt > 0.U) { satCounters(updSet) := cnt - 1.U }
    } .elsewhen (io.satUpdate.bits.inc) {
      when (cnt < satCounterMax) { satCounters(updSet) := cnt + 1.U }
    }
  }

  // SSBC: sc bit maintenance (set on displaced-line insertion)
  when (write.valid && wipeDone && ssbcEnabled && write.bits.data.displaced) {
    secondSearchBits(write.bits.data.originSet) := true.B
    printf("[SSBC] SC_SET: originSet=%d (displaced line inserted into set %d)\n",
           write.bits.data.originSet, write.bits.set)
  }

  // SSBC: conservative sc clear on eviction of last displaced line for an origin set
  val writeInvalidDisplaced = write.valid && wipeDone && ssbcEnabled &&
                              write.bits.data.state === INVALID && write.bits.data.displaced
  when (writeInvalidDisplaced && ren2 && set === write.bits.set) {
    val remainingForOrigin = Cat(ways.zipWithIndex.map { case (w, i) =>
      w.state =/= INVALID && w.displaced && (w.originSet === write.bits.data.originSet) && (i.U =/= write.bits.way)
    }.reverse).orR
    when (!remainingForOrigin) {
      secondSearchBits(write.bits.data.originSet) := false.B
      printf("[SSBC] SC_CLEAR: originSet=%d (no remaining displaced lines in set %d)\n",
             write.bits.data.originSet, write.bits.set)
    }
  }

  // Log directory writes to track displaced bit
  when (write.valid && wipeDone && ssbcEnabled) {
    val writeData = write.bits.data
    printf("[SSBC] DIR_WRITE set=%d way=%d tag=0x%x state=%d dirty=%d displaced=%d originSet=%d source=%d clients=0x%x\n",
           write.bits.set, write.bits.way, writeData.tag, writeData.state, 
           writeData.dirty, writeData.displaced, writeData.originSet, writeData.source, writeData.clients)
  }

  params.ccover(ren2 && setQuash && tagMatch, "DIRECTORY_HIT_BYPASS", "Bypassing write to a directory hit")
  params.ccover(ren2 && setQuash && !tagMatch && wayMatch, "DIRECTORY_EVICT_BYPASS", "Bypassing a write to a directory eviction")

  def json: String = s"""{"clients":${params.clientBits},"mem":"${cc_dir.pathName}","clean":"${wipeDone.pathName}"}"""
}
