
import Helper.SeqDataExtension
import chisel3._
import chisel3.experimental.{ChiselAnnotation, annotate}
import chisel3.internal.firrtl.Width
import chisel3.util.{HasBlackBoxInline, log2Ceil}
import firrtl.annotations.MemoryArrayInitAnnotation




class ReadOnlyTable[T <: Data](banks: Int, init: Seq[T], useVec: Boolean, synchronous: Boolean, clock: Clock, name: Option[String] = None) {

  val width = init.toVec.head.getWidth

  def read(p: UInt): Seq[T] = if (useVec) {
    if(synchronous) getBanks.map(_.toVec.apply(RegNext(p))) else getBanks.map(_.toVec.apply(p))
  } else {
    if(synchronous) {
      val roms = getBanks.zipWithIndex.map { case (bankInit, i) =>
        Module(new SynchronousVerilogRom(
          log2Ceil(init.length / banks).W -> init.toVec.head.getWidth.W,
          bankInit.map(_.litValue), name match {
            case Some(name) => Some(name + i)
            case None => None
          })
        )
      }
      roms.foreach(_.io.index := p)
      roms.foreach(_.io.clock := clock)
      roms.map(_.io.data.asTypeOf(init.toVec.head))
    } else {
      val roms = getBanks.zipWithIndex.map { case (bankInit, i) =>
        Module(new VerilogRom(
          log2Ceil(init.length / banks).W -> init.toVec.head.getWidth.W,
          bankInit.map(_.litValue), name match {
            case Some(name) => Some(name + i)
            case None => None
          })
        )
      }
      roms.foreach(_.io.index := p)
      roms.foreach(_.io.clock := clock)
      roms.map(_.io.data.asTypeOf(init.toVec.head))
    }

  }

  def read(ps: Seq[UInt]): Seq[T] = if (useVec) {
    if(synchronous) getBanks.zip(ps).map{ case (bank, p) => bank.toVec.apply(RegNext(p)) } else getBanks.zip(ps).map{ case (bank, p) => bank.toVec.apply(p) }
  } else {
    if(synchronous) {
      require(ps.length == banks)
      val roms = getBanks.zipWithIndex.map { case (bankInit, i) =>
        Module(new SynchronousVerilogRom(
          log2Ceil(init.length / banks).W -> init.toVec.head.getWidth.W,
          bankInit.map(_.litValue), name match {
            case Some(name) => Some(name + i)
            case None => None
          })
        )
      }
      roms.zip(ps).foreach { case (rom, p) => rom.io.index := p }
      roms.foreach(_.io.clock := clock)
      roms.map(_.io.data.asTypeOf(init.toVec.head))
    } else {
      require(ps.length == banks)
      val roms = getBanks.zipWithIndex.map { case (bankInit, i) =>
        Module(new VerilogRom(
          log2Ceil(init.length / banks).W -> init.toVec.head.getWidth.W,
          bankInit.map(_.litValue), name match {
            case Some(name) => Some(name + i)
            case None => None
          })
        )
      }
      roms.zip(ps).foreach { case (rom, p) => rom.io.index := p }
      roms.foreach(_.io.clock := clock)
      roms.map(_.io.data.asTypeOf(init.toVec.head))
    }
  }

  private def getBanks: Seq[Seq[T]] = init.grouped(banks).map(g => g.padTo(banks, g.head)).toSeq.transpose

}

object ReadOnlyTable {
  def Synchronous[T <: Data](banks: Int, init: Seq[T], name: Option[String] = None)(implicit simulation: Boolean, clock: Clock): ReadOnlyTable[T] =
    new ReadOnlyTable(banks, init, simulation, true, clock, name)

  def Asynchronous[T <: Data](banks: Int, init: Seq[T], name: Option[String] = None)(implicit simulation: Boolean, clock: Clock): ReadOnlyTable[T] =
    new ReadOnlyTable(banks, init, simulation, false, clock, name)
}

object VerilogRom {
  var id = 0
}

class VerilogRom(widths: (Width, Width), init: Seq[BigInt], customName: Option[String]) extends BlackBox with HasBlackBoxInline {

  val io = IO(new Bundle {
    val clock = Input(Clock())
    val index = Input(UInt(widths._1))
    val data = Output(UInt(widths._2))
  })


  override val desiredName = customName.getOrElse(s"VerilogRom${VerilogRom.id}")
  VerilogRom.id += 1
  val lines = init.zipWithIndex.map { case (out, i) => s"|\t$i: data = ${widths._2.get.toInt}'h${out.toString(16)};" }.mkString("\n")

  setInline(name + ".v",
    s"""
      |module $name
      |(
      |  input wire clock,
      |  input wire [${widths._1.get.toInt-1}:0] index,
      |  output reg [${widths._2.get.toInt-1}:0] data
      |);
      |always @(*) case (index)
      $lines
      |default: data = ${widths._2.get.toInt}'b${Seq.fill(widths._2.get.toInt)("x").mkString("")};
      |endcase
      |endmodule
      |""".stripMargin
  )

}

class SynchronousVerilogRom(widths: (Width, Width), init: Seq[BigInt], customName: Option[String]) extends BlackBox with HasBlackBoxInline {

  val io = IO(new Bundle {
    val clock = Input(Clock())
    val index = Input(UInt(widths._1))
    val data = Output(UInt(widths._2))
  })


  override val desiredName = customName.getOrElse(s"VerilogRom${VerilogRom.id}")
  VerilogRom.id += 1
  val lines = init.zipWithIndex.map { case (out, i) => s"|\t$i: data = ${widths._2.get.toInt}'h${out.toString(16)};" }.mkString("\n")

  setInline(name + ".v",
    s"""
       |module $name
       |(
       |  input wire clock,
       |  input wire [${widths._1.get.toInt-1}:0] index,
       |  output reg [${widths._2.get.toInt-1}:0] data
       |);
       |always @(posedge clock) case (index)
      $lines
       |default: data = ${widths._2.get.toInt}'b${Seq.fill(widths._2.get.toInt)("x").mkString("")};
       |endcase
       |endmodule
       |""".stripMargin
  )

}