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

import freechips.rocketchip.subsystem.{SubsystemBankedCoherenceKey}
import freechips.rocketchip.regmapper._
import freechips.rocketchip.tilelink._

class InclusiveCache(
  val cache: CacheParameters,
  val micro: InclusiveCacheMicroParameters,
  control: Option[InclusiveCacheControlParameters] = None
  )(implicit p: Parameters)
    extends LazyModule
{
  val access = TransferSizes(1, cache.blockBytes)
  val xfer = TransferSizes(cache.blockBytes, cache.blockBytes)
  val atom = TransferSizes(1, cache.beatBytes)

  var resourcesOpt: Option[ResourceBindings] = None

  val device: SimpleDevice = new SimpleDevice("cache-controller", Seq("sifive,inclusivecache0", "cache")) {
    def ofInt(x: Int) = Seq(ResourceInt(BigInt(x)))

    override def describe(resources: ResourceBindings): Description = {
      resourcesOpt = Some(resources)

      val Description(name, mapping) = super.describe(resources)
      // Find the outer caches
      val outer = node.edges.out
        .flatMap(_.manager.managers)
        .filter(_.supportsAcquireB)
        .flatMap(_.resources.headOption)
        .map(_.owner.label)
        .distinct
      val nextlevel: Option[(String, Seq[ResourceValue])] =
        if (outer.isEmpty) {
          None
        } else {
          Some("next-level-cache" -> outer.map(l => ResourceReference(l)).toList)
        }

      val extra = Map(
        "cache-level"            -> ofInt(2),
        "cache-unified"          -> Nil,
        "cache-size"             -> ofInt(cache.sizeBytes * node.edges.in.size),
        "cache-sets"             -> ofInt(cache.sets * node.edges.in.size),
        "cache-block-size"       -> ofInt(cache.blockBytes),
        "sifive,mshr-count"      -> ofInt(InclusiveCacheParameters.all_mshrs(cache, micro)))
      Description(name, mapping ++ extra ++ nextlevel)
    }
  }

  val node: TLAdapterNode = TLAdapterNode(
    clientFn  = { _ => TLClientPortParameters(Seq(TLClientParameters(
      name          = s"L${cache.level} InclusiveCache",
      sourceId      = IdRange(0, InclusiveCacheParameters.out_mshrs(cache, micro)),
      supportsProbe = xfer)))
    },
    managerFn = { m => TLManagerPortParameters(
      managers = m.managers.map { m => m.copy(
        regionType         = if (m.regionType >= RegionType.UNCACHED) RegionType.CACHED else m.regionType,
        resources          = Resource(device, "caches") +: m.resources,
        supportsAcquireB   = xfer,
        supportsAcquireT   = if (m.supportsAcquireT) xfer else TransferSizes.none,
        supportsArithmetic = if (m.supportsAcquireT) atom else TransferSizes.none,
        supportsLogical    = if (m.supportsAcquireT) atom else TransferSizes.none,
        supportsGet        = access,
        supportsPutFull    = if (m.supportsAcquireT) access else TransferSizes.none,
        supportsPutPartial = if (m.supportsAcquireT) access else TransferSizes.none,
        supportsHint       = access,
        alwaysGrantsT      = false,
        fifoId             = None)
      },
      beatBytes  = cache.beatBytes,
      endSinkId  = InclusiveCacheParameters.all_mshrs(cache, micro),
      minLatency = 2)
    })

  val ctrls = control.map { c =>
    val nCtrls = if (c.bankedControl) p(SubsystemBankedCoherenceKey).nBanks else 1
    Seq.tabulate(nCtrls) { i => LazyModule(new InclusiveCacheControl(this,
      c.copy(address = c.address + i * InclusiveCacheParameters.L2ControlSize))) }
  }.getOrElse(Nil)

  lazy val module = new Impl
  class Impl extends LazyModuleImp(this) {
    // If you have a control port, you must have at least one cache port
    require (ctrls.isEmpty || !node.edges.in.isEmpty)

    // Extract the client IdRanges; must be the same on all ports!
    val clientIds = node.edges.in.headOption.map(_.client.clients.map(_.sourceId).sortBy(_.start))
    node.edges.in.foreach { e => require(e.client.clients.map(_.sourceId).sortBy(_.start) == clientIds.get) }

    // Use the natural ordering of clients (just like in Directory)
    node.edges.in.headOption.foreach { n =>
      println(s"L${cache.level} InclusiveCache Client Map:")
      n.client.clients.zipWithIndex.foreach { case (c,i) =>
        println(s"\t${i} <= ${c.name}")
      }
      println("")
    }

    // Performance counters for hit/miss tracking
    val hitCounter = RegInit(0.U(64.W))
    val missCounter = RegInit(0.U(64.W))
    val totalAccessCounter = RegInit(0.U(64.W))

    // Create the L2 Banks
    val mods = (node.in zip node.out).zipWithIndex.map { case (((in, edgeIn), (out, edgeOut)), i) =>
      edgeOut.manager.managers.foreach { m =>
        require (m.supportsAcquireB.contains(xfer),
          s"All managers behind the L2 must support acquireB($xfer) " +
          s"but ${m.name} only supports (${m.supportsAcquireB})!")
        if (m.supportsAcquireT) require (m.supportsAcquireT.contains(xfer),
          s"Any probing managers behind the L2 must support acquireT($xfer) " +
          s"but ${m.name} only supports (${m.supportsAcquireT})!")
      }

      val params = InclusiveCacheParameters(cache, micro, !ctrls.isEmpty, edgeIn, edgeOut)
      val scheduler = Module(new InclusiveCacheBankScheduler(params)).suggestName("inclusive_cache_bank_sched")

      scheduler.io.in <> in
      out <> scheduler.io.out
      scheduler.io.ways := DontCare
      scheduler.io.divs := DontCare

      // Log inner TL channel traffic at the L2 boundary (first beat only).
      when (in.a.fire) {
        val prefix = p"[InclusiveCache] L2 bank=$i INNER.A opcode="
        val suffix = p" param=${in.a.bits.param} size=${in.a.bits.size} source=${in.a.bits.source} " +
          p"address=0x${Hexadecimal(in.a.bits.address)}\n"
        when (in.a.bits.opcode === TLMessages.AcquireBlock) {
          printf(prefix + "AcquireBlock" + suffix)
        }.elsewhen (in.a.bits.opcode === TLMessages.AcquirePerm) {
          printf(prefix + "AcquirePerm" + suffix)
        }.elsewhen (in.a.bits.opcode === TLMessages.ArithmeticData) {
          printf(prefix + "ArithmeticData" + suffix)
        }.elsewhen (in.a.bits.opcode === TLMessages.LogicalData) {
          printf(prefix + "LogicalData" + suffix)
        }.elsewhen (in.a.bits.opcode === TLMessages.Get) {
          printf(prefix + "Get" + suffix)
        }.elsewhen (in.a.bits.opcode === TLMessages.Hint) {
          printf(prefix + "Hint" + suffix)
        }.elsewhen (in.a.bits.opcode === TLMessages.PutFullData) {
          printf(prefix + "PutFullData" + suffix)
        }.elsewhen (in.a.bits.opcode === TLMessages.PutPartialData) {
          printf(prefix + "PutPartialData" + suffix)
        }.otherwise {
          printf(prefix + "unknown" + suffix)
        }
      }

      when (in.b.fire) {
        val prefix = p"[InclusiveCache] L2 bank=$i INNER.B opcode="
        val suffix = p" param=${in.b.bits.param} size=${in.b.bits.size} source=${in.b.bits.source} " +
          p"address=0x${Hexadecimal(in.b.bits.address)}\n"
        when (in.b.bits.opcode === TLMessages.PutFullData) {
          printf(prefix + "PutFullData" + suffix)
        }.elsewhen (in.b.bits.opcode === TLMessages.PutPartialData) {
          printf(prefix + "PutPartialData" + suffix)
        }.elsewhen (in.b.bits.opcode === TLMessages.ArithmeticData) {
          printf(prefix + "ArithmeticData" + suffix)
        }.elsewhen (in.b.bits.opcode === TLMessages.LogicalData) {
          printf(prefix + "LogicalData" + suffix)
        }.elsewhen (in.b.bits.opcode === TLMessages.Get) {
          printf(prefix + "Get" + suffix)
        }.elsewhen (in.b.bits.opcode === TLMessages.Hint) {
          printf(prefix + "Hint" + suffix)
        }.elsewhen (in.b.bits.opcode === TLMessages.Probe) {
          printf(prefix + "Probe" + suffix)
        }.otherwise {
          printf(prefix + "unknown" + suffix)
        }
      }

      when (in.c.fire) {
        val prefix = p"[InclusiveCache] L2 bank=$i INNER.C opcode="
        val suffix = p" param=${in.c.bits.param} size=${in.c.bits.size} source=${in.c.bits.source} " +
          p"address=0x${Hexadecimal(in.c.bits.address)}\n"
        when (in.c.bits.opcode === TLMessages.AccessAck) {
          printf(prefix + "AccessAck" + suffix)
        }.elsewhen (in.c.bits.opcode === TLMessages.AccessAckData) {
          printf(prefix + "AccessAckData" + suffix)
        }.elsewhen (in.c.bits.opcode === TLMessages.HintAck) {
          printf(prefix + "HintAck" + suffix)
        }.elsewhen (in.c.bits.opcode === TLMessages.ProbeAck) {
          printf(prefix + "ProbeAck" + suffix)
        }.elsewhen (in.c.bits.opcode === TLMessages.ProbeAckData) {
          printf(prefix + "ProbeAckData" + suffix)
        }.elsewhen (in.c.bits.opcode === TLMessages.Release) {
          printf(prefix + "Release" + suffix)
        }.elsewhen (in.c.bits.opcode === TLMessages.ReleaseData) {
          printf(prefix + "ReleaseData" + suffix)
        }.otherwise {
          printf(prefix + "unknown" + suffix)
        }
      }

      when (in.d.fire) {
        val (_, last, _, beat) = edgeIn.count(in.d)
        val prefix = p"[InclusiveCache] L2 bank=$i INNER.D opcode="
        val suffix = p" param=${in.d.bits.param} size=${in.d.bits.size} source=${in.d.bits.source} sink=${in.d.bits.sink} beat=${beat} last=${last}\n"
        when (in.d.bits.opcode === TLMessages.AccessAck) {
          printf(prefix + "AccessAck" + suffix)
        }.elsewhen (in.d.bits.opcode === TLMessages.AccessAckData) {
          printf(prefix + "AccessAckData" + suffix)
        }.elsewhen (in.d.bits.opcode === TLMessages.Grant) {
          printf(prefix + "Grant" + suffix)
        }.elsewhen (in.d.bits.opcode === TLMessages.GrantData) {
          printf(prefix + "GrantData" + suffix)
        }.elsewhen (in.d.bits.opcode === TLMessages.ReleaseAck) {
          printf(prefix + "ReleaseAck" + suffix)
        }.elsewhen (in.d.bits.opcode === TLMessages.HintAck) {
          printf(prefix + "HintAck" + suffix)
        }.otherwise {
          printf(prefix + "unknown" + suffix)
        }
      }

      when (in.e.fire) {
        val (_, last, _, beat) = edgeIn.count(in.e)
        val prefix = p"[InclusiveCache] L2 bank=$i INNER.E opcode="
        val suffix = p" sink=${in.e.bits.sink} beat=${beat} last=${last}\n"
        printf(prefix + "GrantAck" + suffix)
      }

      // Log outer TL channel traffic (L2 <-> memory side, first beat only).
      when (out.a.fire) {
        val prefix = p"[InclusiveCache] L2 bank=$i OUTER.A opcode="
        val suffix = p" param=${out.a.bits.param} size=${out.a.bits.size} source=${out.a.bits.source} " +
          p"address=0x${Hexadecimal(out.a.bits.address)}\n"
        when (out.a.bits.opcode === TLMessages.AcquireBlock) {
          printf(prefix + "AcquireBlock" + suffix)
        }.elsewhen (out.a.bits.opcode === TLMessages.AcquirePerm) {
          printf(prefix + "AcquirePerm" + suffix)
        }.elsewhen (out.a.bits.opcode === TLMessages.PutFullData) {
          printf(prefix + "PutFullData" + suffix)
        }.elsewhen (out.a.bits.opcode === TLMessages.PutPartialData) {
          printf(prefix + "PutPartialData" + suffix)
        }.elsewhen (out.a.bits.opcode === TLMessages.ArithmeticData) {
          printf(prefix + "ArithmeticData" + suffix)
        }.elsewhen (out.a.bits.opcode === TLMessages.LogicalData) {
          printf(prefix + "LogicalData" + suffix)
        }.elsewhen (out.a.bits.opcode === TLMessages.Get) {
          printf(prefix + "Get" + suffix)
        }.elsewhen (out.a.bits.opcode === TLMessages.Hint) {
          printf(prefix + "Hint" + suffix)
        }.otherwise {
          printf(prefix + "unknown" + suffix)
        }
      }

      when (out.b.fire) {
        val prefix = p"[InclusiveCache] L2 bank=$i OUTER.B opcode="
        val suffix = p" param=${out.b.bits.param} size=${out.b.bits.size} source=${out.b.bits.source} " +
          p"address=0x${Hexadecimal(out.b.bits.address)}\n"
        when (out.b.bits.opcode === TLMessages.PutFullData) {
          printf(prefix + "PutFullData" + suffix)
        }.elsewhen (out.b.bits.opcode === TLMessages.PutPartialData) {
          printf(prefix + "PutPartialData" + suffix)
        }.elsewhen (out.b.bits.opcode === TLMessages.ArithmeticData) {
          printf(prefix + "ArithmeticData" + suffix)
        }.elsewhen (out.b.bits.opcode === TLMessages.LogicalData) {
          printf(prefix + "LogicalData" + suffix)
        }.elsewhen (out.b.bits.opcode === TLMessages.Get) {
          printf(prefix + "Get" + suffix)
        }.elsewhen (out.b.bits.opcode === TLMessages.Hint) {
          printf(prefix + "Hint" + suffix)
        }.elsewhen (out.b.bits.opcode === TLMessages.Probe) {
          printf(prefix + "Probe" + suffix)
        }.otherwise {
          printf(prefix + "unknown" + suffix)
        }
      }

      when (out.c.fire) {
        val prefix = p"[InclusiveCache] L2 bank=$i OUTER.C opcode="
        val suffix = p" param=${out.c.bits.param} size=${out.c.bits.size} source=${out.c.bits.source} " +
          p"address=0x${Hexadecimal(out.c.bits.address)}\n"
        when (out.c.bits.opcode === TLMessages.AccessAck) {
          printf(prefix + "AccessAck" + suffix)
        }.elsewhen (out.c.bits.opcode === TLMessages.AccessAckData) {
          printf(prefix + "AccessAckData" + suffix)
        }.elsewhen (out.c.bits.opcode === TLMessages.HintAck) {
          printf(prefix + "HintAck" + suffix)
        }.elsewhen (out.c.bits.opcode === TLMessages.ProbeAck) {
          printf(prefix + "ProbeAck" + suffix)
        }.elsewhen (out.c.bits.opcode === TLMessages.ProbeAckData) {
          printf(prefix + "ProbeAckData" + suffix)
        }.elsewhen (out.c.bits.opcode === TLMessages.Release) {
          printf(prefix + "Release" + suffix)
        }.elsewhen (out.c.bits.opcode === TLMessages.ReleaseData) {
          printf(prefix + "ReleaseData" + suffix)
        }.otherwise {
          printf(prefix + "unknown" + suffix)
        }
      }

      when (out.d.fire) {
        val (_, last, _, beat) = edgeOut.count(out.d)
        val prefix = p"[InclusiveCache] L2 bank=$i OUTER.D opcode="
        val suffix = p" param=${out.d.bits.param} size=${out.d.bits.size} source=${out.d.bits.source} sink=${out.d.bits.sink} beat=${beat} last=${last}\n"
        when (out.d.bits.opcode === TLMessages.AccessAck) {
          printf(prefix + "AccessAck" + suffix)
        }.elsewhen (out.d.bits.opcode === TLMessages.AccessAckData) {
          printf(prefix + "AccessAckData" + suffix)
        }.elsewhen (out.d.bits.opcode === TLMessages.HintAck) {
          printf(prefix + "HintAck" + suffix)
        }.elsewhen (out.d.bits.opcode === TLMessages.Grant) {
          printf(prefix + "Grant" + suffix)
        }.elsewhen (out.d.bits.opcode === TLMessages.GrantData) {
          printf(prefix + "GrantData" + suffix)
        }.elsewhen (out.d.bits.opcode === TLMessages.ReleaseAck) {
          printf(prefix + "ReleaseAck" + suffix)
        }.otherwise {
          printf(prefix + "unknown" + suffix)
        }
      }

      when (out.e.fire) {
        val (_, last, _, beat) = edgeOut.count(out.e)
        val prefix = p"[InclusiveCache] L2 bank=$i OUTER.E opcode="
        val suffix = p" sink=${out.e.bits.sink} beat=${beat} last=${last}\n"
        printf(prefix + "GrantAck" + suffix)
      }

      // Tie down default values in case there is no controller
      scheduler.io.req.valid := false.B
      scheduler.io.req.bits.address := 0.U
      scheduler.io.req.bits.invalidate := false.B
      scheduler.io.resp.ready := true.B

      // Connect bankDisable from control register if present
      if (ctrls.nonEmpty && i < ctrls.length) {
        scheduler.io.bankDisable := ctrls(i).module.io.bankDisable
      } else {
        scheduler.io.bankDisable := 0.U
      }

      // Fix-up the missing addresses. We do this here so that the Scheduler can be
      // deduplicated by Firrtl to make hierarchical place-and-route easier.
      out.a.bits.address := params.restoreAddress(scheduler.io.out.a.bits.address)
      in .b.bits.address := params.restoreAddress(scheduler.io.in .b.bits.address)
      out.c.bits.address := params.restoreAddress(scheduler.io.out.c.bits.address)

      // Integrate TLMonitor for debug
      val monitor = Module(new freechips.rocketchip.tilelink.TLMonitor(
        freechips.rocketchip.tilelink.TLMonitorArgs(edgeIn)
      ))
      monitor.io.in := in

      scheduler
    }

    // Aggregate performance counters from all banks
    val bankHits = mods.map(_.io.perf.access_hit)
    val bankAccesses = mods.map(_.io.perf.access_valid)
    
    // Count total hits, misses, and accesses across all banks
    val cycleHits = PopCount(bankHits)
    val cycleAccesses = PopCount(bankAccesses)
    val cycleMisses = cycleAccesses - cycleHits
    
    // Update counters
    hitCounter := hitCounter + cycleHits
    missCounter := missCounter + cycleMisses  
    totalAccessCounter := totalAccessCounter + cycleAccesses

    ctrls.foreach { ctrl =>
      ctrl.module.io.flush_req.ready := false.B
      ctrl.module.io.flush_resp := false.B
      ctrl.module.io.flush_match := false.B
      // Connect performance counters to control interface
      ctrl.module.io.hit_count := hitCounter
      ctrl.module.io.miss_count := missCounter
      ctrl.module.io.total_access_count := totalAccessCounter
    }

    mods.zip(node.edges.in).zipWithIndex.foreach { case ((sched, edgeIn), i) =>
      val ctrl = if (ctrls.size > 1) Some(ctrls(i)) else ctrls.headOption
      ctrl.foreach { ctrl => {
        val contained = edgeIn.manager.managers.flatMap(_.address)
          .map(_.contains(ctrl.module.io.flush_req.bits)).reduce(_||_)
        when (contained) { ctrl.module.io.flush_match := true.B }

        sched.io.req.valid := contained && ctrl.module.io.flush_req.valid
        sched.io.req.bits.address := ctrl.module.io.flush_req.bits
        sched.io.req.bits.invalidate := ctrl.module.io.invalidate_req
        when (contained && sched.io.req.ready) { ctrl.module.io.flush_req.ready := true.B }

        when (sched.io.resp.valid) { ctrl.module.io.flush_resp := true.B }
        sched.io.resp.ready := true.B
      }}
    }

    def json = s"""{"banks":[${mods.map(_.json).mkString(",")}]}"""
  }
}
