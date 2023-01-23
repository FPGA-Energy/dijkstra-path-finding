
import chisel3._
import chisel3.internal.firrtl.Width
import Helper._
import RouteMemory.Entry
import chisel3.util.log2Ceil

class RouteMemory(n: Int, k: Int, w: Width) extends Module {

  val io = IO(new RouteMemory.IO(n, k, w))

  val routeMemory = SyncReadMem(n / k, Vec(k, RouteMemory.RouteInfo(n, w)))
  val validReg = Reg(Vec(n / k, Vec(k, Bool())))
  val visitedReg = Reg(Vec(n / k, Vec(k, Bool())))


  io.entries := (
    RegNext(visitedReg(io.sinkGroup)),
    RegNext(validReg(io.sinkGroup)),
    routeMemory.read(io.sinkGroup)
  )
    .zipped
    .map { case (visited, valid, route) => Entry(visited, Valid(valid, route)) }
    .toVec

  val offset = io.visit.data.id.takeLsb(log2Ceil(k))
  val base = io.visit.data.id.dropLsb(log2Ceil(k))
  when(io.visit.valid) {
    visitedReg(base)(offset) := 1.B
  }

  val Pair(updateGroup, updates) = io.update
  validReg(updateGroup).zip(updates).foreach { case (validReg, Valid(valid, _)) =>
    when(valid) { validReg := 1.B }
  }
  routeMemory.write(
    updateGroup,
    updates.map(_.data).toVec,
    updates.map(_.valid).toVec
  )

  when(io.reinitialize) {
    validReg := VecInit.fill(n / k, k)(0.B)
    visitedReg := VecInit.fill(n / k, k)(0.B)
  }

}


object RouteMemory {

  class RouteInfo(n: Int, w: Width) extends Bundle {
    val predecessor = Graph.Node(n)
    val distance = UInt(w)
  }
  object RouteInfo {
    def apply(n: Int, w: Width): RouteInfo = new RouteInfo(n, w)
    def apply(predecessor: Graph.Node, distance: UInt): RouteInfo =
      Wire(RouteInfo(predecessor.n, distance.getWidth.W)).expand(
        _.predecessor := predecessor,
        _.distance := distance
      )
    def unapply(r: RouteInfo): Option[(Graph.Node, UInt)] = Some(r.predecessor, r.distance)
  }
  class Entry(n: Int, w: Width) extends Bundle {
    val visited = Bool()
    val route = Valid(RouteInfo(n, w))
  }
  object Entry {
    def apply(n: Int, w: Width): Entry = new Entry(n, w)
    def apply(visited: Bool, route: Valid[RouteInfo]): Entry = Wire(Entry(route.data.predecessor.n, route.data.distance.getWidth.W)).expand(
      _.visited := visited,
      _.route := route
    )
    def unapply(e: Entry): Option[(Bool, Valid[RouteInfo])] = Some(e.visited, e.route)
  }

  class IO(n: Int, k: Int, w: Width) extends Bundle {
    val reinitialize = Input(Bool())
    val sinkGroup = Input(UInt(0 until n / k))
    val entries = Output(Vec(k, RouteMemory.Entry(n, w)))
    val update = Input(Pair(UInt(0 until n / k), Vec(k, Valid(RouteInfo(n, w)))))
    val visit = Input(Valid(Graph.Node(n)))
  }

  def apply(n: Int, k: Int, w: Width): RouteMemory = Module(new RouteMemory(n, k, w))


  def main(args: Array[String]) = emitVerilog(new RouteMemory(1024, 4, 16.W), Array("--target-dir", "build"))
}