
import DijkstraCore.State
import chisel3._
import Helper._
import RouteMemory.{Entry, RouteInfo}
import RunningMinimum.Candidate
import chisel3.experimental.ChiselEnum
import chisel3.internal.firrtl.Width
import chisel3.util.{Decoupled, is, log2Ceil, switch}
import firrtl.transforms.DontTouchAnnotation

class DijkstraCore(k: Int, g: Graph)(implicit simulation: Boolean) extends Module {

  val io = IO(new DijkstraCore.IO(g.n, g.w))


  val stateReg = RegInit(State.Startup)

  val sinkGroupReg = Reg(UInt(0 until g.n / k))
  val delayedSinkGroupReg = RegNext(sinkGroupReg, 0.U)
  val weightMemory = Module(new WeightMemory(g.getSinkTable.length, k, g))
  val routeMemory = Module(new RouteMemory(g.n, k, g.w))

  val current = Reg(Candidate(g.n, g.w))
  val distance = Reg(UInt(0 until (g.n * g.w.maxValue.toInt)))
  val start = Reg(Graph.Node(g.n))
  val end = Reg(Graph.Node(g.n))



  val routeUpdates: Seq[(Valid[RouteInfo], Valid[Candidate])] = weightMemory.io.weights.data
    .zip(routeMemory.io.entries)
    .zipWithIndex
    .map { case ((Valid(connected, weight), Entry(visited, Valid(known, oldRoute))), i) =>
      val newDistance = current.distance +& weight
      val newRoute = RouteInfo(current.node, newDistance.takeLsb(g.w.get.toInt))
      val overflow = WireDefault(newRoute.distance(newRoute.distance.getWidth-1))
      val shorterRoute = newDistance < oldRoute.distance
      val update = (!known || shorterRoute) && connected
      val route = Mux(update, newRoute, oldRoute)
      (
        Valid(update && !overflow && weightMemory.io.weights.valid, route),
        Valid(!visited && (known || update) && weightMemory.io.weights.valid, Candidate(
          Graph.Node(g.n, delayedSinkGroupReg ## i.U(log2Ceil(k).W)),
          route.distance))
      )
    }

  routeMemory.io.update := Pair(
    delayedSinkGroupReg,
    routeUpdates.map(_._1).toVec
  )

  val (minimum, resetMinimum) = RunningMinimum(routeUpdates.map(t => RegNext(t._2)))


  val lastGroup = sinkGroupReg === ((g.n / k) - 1).U

  weightMemory.io.sinkGroup := Valid(0.B, sinkGroupReg)
  weightMemory.io.changeSource := Valid(0.B, minimum.data.node)
  routeMemory.io.visit := Valid(0.B, minimum.data.node)
  routeMemory.io.sinkGroup := sinkGroupReg
  routeMemory.io.reinitialize := 0.B

  io.route.ready := 0.B
  io.distance.valid := 0.B
  io.distance.data := distance

  switch(stateReg) {

    is(State.Startup) {

      routeMemory.io.reinitialize := 1.B

      stateReg := State.Idle

    }

    is(State.Idle) {

      io.route.ready := 1.B
      io.distance.valid := 1.B

      start := io.route.bits.start
      end := io.route.bits.end
      current := Candidate(io.route.bits.start, 0.U)
      val isNewRoute = io.route.bits.start =/= start || io.route.bits.end =/= end

      when(isNewRoute && io.route.valid) {
        stateReg := State.SetStartDistance
        weightMemory.io.changeSource := Valid(1.B, io.route.bits.start)
      }

    }
    is(State.SetStartDistance) {
      routeMemory.io.update := Pair(
        start.id.dropLsb(log2Ceil(k)),
        VecInit.tabulate(k) { i => Valid(start.id.takeLsb(log2Ceil(k)) === i.U, RouteInfo(start, 0.U)) }
      )
      routeMemory.io.visit := Valid(1.B, io.route.bits.start)
      stateReg := State.Update
      sinkGroupReg := 0.U
      weightMemory.io.sinkGroup.valid := 1.B
    }
    is(State.Update) {

      stateReg := State.Update

      weightMemory.io.sinkGroup.valid := 1.B
      when(weightMemory.io.ready) {
        sinkGroupReg := sinkGroupReg + 1.U
      }

      when(lastGroup) {
        stateReg := State.WaitForMinimum
      }

    }
    is(State.WaitForMinimum) {
      stateReg := State.WaitForMinimum2
    }
    is(State.WaitForMinimum2) {
      stateReg := State.ChooseNext
    }
    is(State.ChooseNext) {

      sinkGroupReg := 0.U
      resetMinimum := 1.B
      current := minimum.data
      weightMemory.io.changeSource.valid := 1.B
      routeMemory.io.visit.valid := 1.B



      stateReg := Mux(minimum.valid, State.Update, State.ExtractDistance)

    }
    is(State.ExtractDistance) {
      routeMemory.io.sinkGroup := end.id.dropLsb(log2Ceil(k))
      stateReg := State.ExtractDistance2
    }
    is(State.ExtractDistance2) {
      routeMemory.io.reinitialize := 1.B
      distance := routeMemory.io.entries(end.id.takeLsb(log2Ceil(k))).route.data.distance
      stateReg := State.Idle
    }
  }



}

object DijkstraCore {

  object State extends ChiselEnum {
    val Startup, Idle, SetStartDistance, Update, WaitForMinimum, WaitForMinimum2, ChooseNext, ExtractDistance, ExtractDistance2 = Value
  }

  class IO(n: Int, w: Width) extends Bundle {
    val route = Flipped(Decoupled(new Bundle {
      val start = Graph.Node(n)
      val end = Graph.Node(n)
    }))
    val distance = Output(Valid(UInt(0 until (n * w.maxValue.toInt))))
  }

  def main(args: Array[String]) = emitVerilog(new DijkstraCore(4, Graph.fromFile("src/graphs/d-d-64-v.csv", 16.W))(false), Array("--target-dir","build"))

}