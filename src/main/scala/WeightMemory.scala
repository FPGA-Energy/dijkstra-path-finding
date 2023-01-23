
import chisel3._
import chisel3.internal.firrtl.Width
import chisel3.util.{PopCount, PriorityEncoderOH, RegEnable, log2Ceil, MuxCase}
import Helper._


class WeightMemory(size: Int, k: Int, g: Graph)(implicit simulation: Boolean) extends Module {
  implicit val clk = clock

  val io = IO(new WeightMemory.IO(g.n, k, g.w))

  val hitCount = Wire(UInt(0 to k))
  val (availableSinks, addresses, offset) = AvailableSinks(size, k, g)(io.changeSource, hitCount)

  val hits = availableSinks.data.map { case Valid(valid, node) =>
    valid && io.sinkGroup.valid && availableSinks.valid && node.id.dropLsb(log2Ceil(k)) === io.sinkGroup.data
  }
  hitCount := PopCount(hits)


  val weightTable = ReadOnlyTable.Synchronous(k, g.getWeightTable, Some("WeightTable"))
  val weights = weightTable.read(addresses)

  val matchers = Seq.tabulate(k) { i =>
    (hits, availableSinks.data, weights)
      .zipped
      .map { case (hit, Valid(_, node), weight) =>
        val subhit = WireDefault((node.id.takeLsb(log2Ceil(k)) === i.U) && hit)
        RegNext(subhit, 0.B) -> weight
      }
  }

  io.ready := availableSinks.valid
  io.weights := Valid(RegNext(availableSinks.valid && io.sinkGroup.valid, 0.B),
    matchers.map { cases =>
      Valid(cases.map(_._1).toVec.reduceTree(_ || _), MuxCase(DontCare, cases))
    }.toVec
  )
}

class BadWeightMemory(size: Int, k: Int, g: Graph)(implicit simulation: Boolean) extends Module {

  val io = IO(new WeightMemory.IO(g.n, k, g.w))

  val source = RegEnable(io.changeSource.data, Graph.Node(g.n, 0), io.changeSource.valid)

  val (connected, weight) = g.adjacencyMatrix

  println(weight.map(_.map(_.litValue).mkString(", ")).mkString("\n"))
  println(weight.map(_.toVec).toVec)

  val weightVec = weight.map(_.toVec).toVec
  val connectedVec = connected.map(_.toVec).toVec

  val weights = Seq.tabulate(k) { i =>
    val index = WireDefault(io.sinkGroup.data ## i.U(log2Ceil(k).W))
    Valid(
      connectedVec(source.id)(index),
      weightVec(source.id)(index)
    )
  }.toVec

  io.ready := !io.changeSource.valid

  io.weights := Valid(RegNext(io.sinkGroup.valid, 0.B) && !RegNext(io.changeSource.valid, 0.B), RegNext(weights))

}


object WeightMemory {

  class IO(n: Int, k: Int, w: Width) extends Bundle {
    val changeSource = Input(Valid(Graph.Node(n)))
    val sinkGroup = Input(Valid(UInt(0 until n/k)))
    val ready = Output(Bool())
    val weights = Output(Valid(Vec(k, Valid(UInt(w)))))
  }

  def main(args: Array[String]) = emitVerilog(new WeightMemory(16384, 4, Graph.fromFile("src/graphs/d-d-64-v.csv", 16.W))(false), Array("--target-dir","build"))


}

