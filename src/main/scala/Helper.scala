
import chisel3._
import chisel3.experimental.BundleLiterals.AddBundleLiteralConstructor
import chisel3.experimental.{ChiselAnnotation, annotate, requireIsHardware}
import chisel3.internal.firrtl.Width
import chisel3.util._
import firrtl.annotations.MemoryArrayInitAnnotation

import scala.annotation.tailrec
import scala.util.Random

object Helper {

  implicit class SeqDataExtension[T <: Data](x: Seq[T]) {
    def toVec: Vec[T] = VecInit(x)
    def reducePipelined(reductionPerStage: Int)(fun: (T,T) => T): T = pipelinedReduce(VecInit(x), reductionPerStage, fun)
  }

  implicit class WidthExtension(w: Width) {
    def maxValue: BigInt = BigInt(math.pow(2, w.get.toInt).toLong - 1)
  }

  implicit class Replicator[T](x: T) {
    def repeat(k: Int): Seq[T] = Seq.fill(k)(x)
  }

  implicit class UIntFactoryExtension(x: chisel3.UInt.type) {
    def apply(range: Range): UInt = UInt(log2Ceil(range.max + 1).W)
  }
  implicit class UIntExtension(x: UInt) {
    def dropLsb(n: Int = 1): UInt = x(x.getWidth - 1, n)
    def takeLsb(n: Int = 1): UInt = x(n - 1, 0)
    def dropMsb(n: Int = 1): UInt = x(x.getWidth - 2, 0)
    def takeMsb(n: Int = 1): UInt = x(x.getWidth - 1, x.getWidth - 1 - n)

    def maxValue: UInt = x.getWidth.W.maxValue.U
  }

  implicit class Transformer[T](x: T) {
    def |>[B](fun: T => B): B = fun(x)
  }

  object MuxElementWise {
    def apply[T <: Data](conds: Seq[Bool], cons: Seq[T], alts: Seq[T]): Seq[T] = {
      require(cons.length == conds.length && alts.length == conds.length)
      (conds, cons, alts)
        .zipped
        .map { case (cond, con, alt) => Mux(cond, con, alt) }
    }
  }

  implicit class BundleExpander[T <: Bundle](b: T) {
    def expand(assignments: T => Any*): T = {
      assignments.foreach(f => f(b))
      b
    }
  }

  implicit class SyncReadMemExtension(x: chisel3.SyncReadMem.type) {
    def apply(width: Width, init: Seq[BigInt]): SyncReadMem[UInt] = {
      val mem = SyncReadMem(init.length, UInt(width))
      annotate(new ChiselAnnotation {
        override def toFirrtl = MemoryArrayInitAnnotation(mem.toTarget, init)
      })
      mem
    }
    def apply(width: Width, init: Seq[Seq[BigInt]]): Seq[SyncReadMem[UInt]] = init.map(SyncReadMem(width, _))
  }

  implicit class MemExtension(x: chisel3.Mem.type) {
    def apply(width: Width, init: Seq[BigInt]): Mem[UInt] = {
      val mem = Mem(init.length, UInt(width))
      annotate(new ChiselAnnotation {
        override def toFirrtl = MemoryArrayInitAnnotation(mem.toTarget, init)
      })
      mem
    }

    def apply(width: Width, init: Seq[Seq[BigInt]]): Seq[Mem[UInt]] = init.map(Mem(width, _))
  }

  implicit class RegEnableExtension(x: chisel3.util.RegEnable.type) {
    def apply[T <: Data](init: T)(en: Bool)(next: T => T): T = {
      val reg = RegInit(init)
      when(en) { reg := next(reg) }
      reg
    }
  }

  object Register {
    def apply[T <: Data](t: T, init: T, en: Bool)(next: T => T): T = {
      val reg = RegInit(t, init)
      when(en) { reg := next(reg) }
      reg
    }
    def apply[T <: Data](t: T, init: T)(next: T => T): T = {
      val reg = RegInit(t, init)
      reg := next(reg)
      reg
    }

  }

  implicit class VecExtension[T <: Data](x: Vec[T]) {

  }

  def pow2(i: Int): Int = scala.math.pow(2, i).toInt

  @tailrec
  def shifted[T](seq: Seq[T], shiftIn: Seq[T], i: Int): Seq[T] = {
    if(i == 0) seq else shifted(shiftIn.last +: seq, shiftIn.dropRight(1), i - 1)
  }

  @tailrec
  def rotated[T](seq: Seq[T], i: Int): Seq[T] = {
    i match {
      case 0 => seq
      case _ => rotated(seq.last +: seq.dropRight(1), i - 1)
    }
  }

  object BarrelShifter {
    def apply[T <: Data](in: Vec[T], shiftIn: Vec[T], shamt: UInt): Vec[T] = {
      barrelShift(in, shiftIn, shamt.asBools, 1, 0.U.asTypeOf(in.head)) { case (d,l,r) =>
        Mux(d,l,r)
      }.toVec
    }
  }

  object BarrelRotater {
    def apply[T <: Data](in: Vec[T], shamt: UInt): Vec[T] = {
      barrelRotate(in, shamt.asBools, 1) { case (d,l,r) => Mux(d,l,r) }.toVec
    }
  }

  @tailrec
  def barrelShift[T,D](in: Seq[T], shiftIn: Seq[T], ds: Seq[D], i: Int, zero: T)(sel: (D,T,T) => T): Seq[T] = {

    if(ds.isEmpty) {
      in
    } else {
      val shift = shifted(in, shiftIn, i)
      val muxed = in.padTo(shift.length, zero).zip(shift).map { case (noShift, shift) =>
        sel(ds.head, shift, noShift)
      }
      barrelShift(muxed, shiftIn.dropRight(i), ds.tail, i << 1, zero)(sel)
    }

  }

  def barrelRotate[T,D](in: Seq[T], ds: Seq[D], i: Int)(sel: (D,T,T) => T): Seq[T] = {
    ds match {
      case Seq() => in
      case d::_ =>
        val rot = rotated(in, i)
        val muxed = in.zip(rot).map { case (noShift, shift) => sel(d, shift, noShift) }
        barrelRotate(muxed, ds.tail, i << i)(sel)
    }
  }


  class Pair[A <: Data, B <: Data](t1: A, t2: B) extends Bundle {
    val _1 = t1
    val _2 = t2
  }
  object Pair {
    def apply[A <: Data, B <: Data](t1: A, t2: B): Pair[A,B] = {
      try {
        requireIsHardware(t1)
        requireIsHardware(t2)
        Wire(new Pair(chiselTypeOf(t1), chiselTypeOf(t2))).expand(
          _._1 := t1,
          _._2 := t2
        )
      } catch {
        case e: ExpectedHardwareException => new Pair(t1, t2)
      }
    }
    def unapply[A <: Data, B <: Data](p: Pair[A,B]): Option[(A,B)] = Some(p._1 -> p._2)
  }

  class Valid[T <: Data](t: T) extends Bundle {
    val valid = Bool()
    val data = t
  }
  object Valid {
    def apply[T <: Data](t: T): Valid[T] = new Valid(t)
    def apply[T <: Data](valid: Bool, data: T): Valid[T] = {
      val w = Wire(Valid(chiselTypeOf(data)))
      w.expand(
        _.valid := valid,
        _.data := data
      )
    }
    def unapply[T <: Data](v: Valid[T]): Option[(Bool, T)] = Some(v.valid, v.data)
  }


  @tailrec
  def pipelinedReduce[T <: Data](x: Vec[T], groups: Int, fun: (T, T) => T): T = {
    if (x.length <= groups) reduce(x, fun)
    else pipelinedReduce(RegNext(
      VecInit(x.grouped(groups).map(g => reduce(g, fun)).toSeq)
    ), groups, fun)
  }

  def reduce[T <: Data](xs: Seq[T], fun: (T, T) => T): T = {
    xs match {
      case Seq(x) => x
      case Seq(x, y) => fun(x, y)
      case _ => reduce(xs.grouped(2).map(reduce(_, fun)).toSeq, fun)
    }
  }

  object RandBool {
    def apply(): Bool = Random.nextBoolean().B
  }
  object RandUInt {
    def apply(range: Range): UInt = (Random.nextInt(range.length) + range.min).U
    def apply(width: Width): UInt = apply(0 until width.maxValue.toInt)
  }


  def nextPow2(x: Int): Int = pow2(log2Ceil(x))

  def roundAt(p: Int)(n: Double): Double = { val s = math pow (10, p); (math round n * s) / s }


}


object dg extends App {


  println(
    Seq(
      Seq(false, false, false),
      Seq(true, false, false),
      Seq(false, true, false),
      Seq(true, true, false),
      Seq(false, false, true),
      Seq(true, false, true),
      Seq(false, true, true),
      Seq(true, true, true)
    ).map { i =>
      Helper.barrelRotate(
        Seq(0,1,2,3,4,5,6,7),
        i, 1) { case (c, l, r) =>
        if(c) l else r
      }}.mkString("\n"))



}