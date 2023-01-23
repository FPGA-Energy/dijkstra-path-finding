

import chisel3._
import Helper._
import RunningMinimum.{Candidate, minimum}
import chisel3.internal.firrtl.Width

class RunningMinimum(n: Int, k: Int, w: Width) extends Module {

  val io = IO(new Bundle {
    val candidates = Input(Vec(k, Valid(Candidate(n, w))))
    val minimum = Output(Valid(Candidate(n, w)))
    val reinitialize = Input(Bool())
  })


  val candidatesMininum = io.candidates.reduceTree(minimum)

  val currentMinimum = Reg(Valid(Candidate(n,w)))
  val nextMinimum = minimum(currentMinimum, RegNext(candidatesMininum))
  currentMinimum := nextMinimum

  io.minimum := nextMinimum

  when(io.reinitialize) {
    currentMinimum := Valid(0.B, 0.U.asTypeOf(Candidate(n,w)))
  }



}


object RunningMinimum {

  def minimum(l: Valid[Candidate], r: Valid[Candidate]): Valid[Candidate] = Mux((r.data.distance < l.data.distance && r.valid) || !l.valid, r, l)


  class Candidate(n: Int, w: Width) extends Bundle {
    val node = Graph.Node(n)
    val distance = UInt(w)
  }
  object Candidate {
    def apply(n: Int, w: Width): Candidate = new Candidate(n, w)
    def apply(node: Graph.Node, distance: UInt): Candidate = Wire(Candidate(node.n, distance.getWidth.W)).expand(
      _.node := node,
      _.distance := distance
    )
  }


  def apply(candidates: Seq[Valid[Candidate]]): (Valid[Candidate], Bool) = {
    val runningMinimum = Module(new RunningMinimum(candidates.head.data.node.n, candidates.length, candidates.head.data.distance.getWidth.W))
    runningMinimum.io.candidates := candidates
    val reinitialize = WireDefault(0.B)
    runningMinimum.io.reinitialize := reinitialize
    runningMinimum.io.minimum -> reinitialize
  }

}