

import Artix7Top.{frequencyMap, lowHighs}
import chisel3._
import chisel3.util._
import Xilinx._

import scala.io.Source

class Artix7Top(k: Int, g: Graph) extends Module {

  val io = IO(new Bundle {
    val leds = Output(UInt(2.W))
    val rgb = Output(UInt(3.W))
  })




  val (lowCycles, highCycles) = lowHighs(k)
  println(lowCycles -> highCycles)

  withClock(MMCME2_ADV(clock,reset, 12.0 -> frequencyMap(k))) {

    val experiment = Module(new ExperimentTop(k, g)(lowCycles, highCycles))
    io.leds := Fill(3, experiment.io.blink)
    io.rgb := Fill(3, !experiment.io.blink)

  }

}

object Artix7Top extends App {


  val frequencyMap = Map(
    2 -> 130,
    4 -> 125,
    8 -> 90,
    16 -> 80,
    32 -> 75
  )

  def fun(repsPerSec: Int) = (repsPerSec * 5, repsPerSec)


  val lowHighs = Map(
    2 -> fun(800),
    4 -> fun(1350),
    8 -> fun(1300),
    16 -> fun(1800),
    32 -> fun(2000)
  )





  val defaultK = 16
  val defaultTestFile = "src/graphs/s-ud-64-v.csv"

  val k = if (args.contains("-k")) args(args.indexOf("-k") + 1).toInt else defaultK
  val w = if (args.contains("-w")) args(args.indexOf("-w") + 1).toInt else 19
  val targetDir = if (args.contains("--target-dir")) args(args.indexOf("--target-dir") + 1) else "build"
  val graph = {
    val testFile = if (args.contains("--test-file")) args(args.indexOf("--test-file") + 1) else defaultTestFile
    Graph.fromFile(testFile, w.W)
  }



  println("k, n, low, high, frequency")
  println(lowHighs.map { case (k, (low, high)) =>
    s"$k, $low, $high, ${frequencyMap(k)*1000000}"
  }.mkString("\n"))

  emitVerilog(new Artix7Top(k, graph), Array("--target-dir", targetDir))
}