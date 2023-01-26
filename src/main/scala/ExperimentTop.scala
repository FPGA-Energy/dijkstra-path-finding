
import chisel3._
import Helper._
import chisel3.util.random.LFSR

import scala.math
class ExperimentTop(k: Int, g: Graph)(lowCycles: Int, highCycles: Int) extends Module {

  val io = IO(new Bundle {
    val blink = Output(Bool())
  })


  val nodeCounter = RegInit(UInt(0 until g.n), 0.U)
  val endNode = RegInit(UInt(0 until g.n), lowCycles.U)
  val repetitionCounter = RegInit(UInt(0 until math.max(lowCycles, highCycles)), 0.U)
  val blinkReg = RegInit(0.B)
  io.blink := blinkReg

  val dijkstraCore = Module(new DijkstraCore(k, g)(simulation = false))

  dijkstraCore.io.route.expand(
    _.valid := 1.B,
    _.bits.expand(
      _.start := Graph.Node(g.n, nodeCounter),
      _.end := Graph.Node(g.n, endNode)
    )
  )

  when(dijkstraCore.io.route.ready) {
    nodeCounter := nodeCounter + 1.U

    when(nodeCounter === nodeCounter.maxValue) {
      repetitionCounter := repetitionCounter - 1.U
      when(repetitionCounter === 0.U) {
        blinkReg := !blinkReg
        repetitionCounter := Mux(blinkReg, lowCycles.U, highCycles.U)
      }
    }
  }

}
