
import AvailableSinks.{bufferSelectionMask, inversion}
import chisel3._
import Helper._
import chisel3.experimental.ChiselEnum
import chisel3.util.{is, log2Ceil, switch}

class AvailableSinks(size: Int, k: Int, g: Graph)(implicit simulation: Boolean) extends Module {

  import AvailableSinks.State
  implicit val clk: Clock = clock

  val io = IO(new Bundle {
    val changeSource = Input(
      Valid(Graph.Node(g.n))
    )
    val availableSinks = Output(
      Valid(Vec(k, Valid(Graph.Node(g.n))))
    )
    val addresses = Output(
      Vec(k, UInt(0 until size / k))
    )
    val offset = Output(UInt(0 until k))
    val hits = Input(UInt(0 to k))
  })



  val stateReg = RegInit(State.Wait)

  val sinkTable = ReadOnlyTable.Synchronous(k, g.getSinkTable, Some("SinkTable"))

  val pointer = RegInit(UInt(0 until size / k), 0.U)
  val nextPointer = WireDefault(pointer)
  pointer := nextPointer
  val startPointer = RegInit(UInt(0 until size), 0.U)
  val endPointer = RegInit(UInt(0 until size), 0.U)

  val hitCounter = RegInit(UInt(0 until k), 0.U)
  val hitCounterUpdate = hitCounter +& io.hits
  val doBufferSwitch = hitCounterUpdate >= k.U
  hitCounter := Mux(io.changeSource.valid, 0.U, hitCounterUpdate)

  val bufferSelect = RegInit(UInt(1.W), 0.U)
  val bufferLoad = WireDefault(VecInit(0.B, 0.B))
  val buffers = Reg(Vec(2, Vec(k, Graph.Node(g.n))))
  val addressBuffers = Reg(Vec(2, UInt(0 until size / k)))
  val insideBoundsBuffers = Reg(Vec(2, Vec(k, Bool())))


  val (startPointerTable, endPointerTable) = g.generatePointerTable
  val sinkMemoryOut = sinkTable.read(nextPointer).toVec

  val nextInsideBounds = Seq.tabulate(k)(i => pointer ## i.U(log2Ceil(k).W)).map { address =>
    (startPointer <= address) && (address <= endPointer)
  }

  val mask = bufferSelectionMask(k).apply(hitCounter).map { b => Mux(bufferSelect.asBool, !b, b) }.toSeq

  val sinks = MuxElementWise(mask, buffers(1), buffers(0))
  val addresses = MuxElementWise(mask, addressBuffers(1).repeat(k), addressBuffers(0).repeat(k))
  val insideBounds = MuxElementWise(mask, insideBoundsBuffers(1), insideBoundsBuffers(0))

  when(bufferLoad(1)) {
    buffers(1) := sinkMemoryOut
    addressBuffers(1) := pointer
    insideBoundsBuffers(1) := nextInsideBounds
  }
  when(bufferLoad(0)) {
    buffers(0) := sinkMemoryOut
    addressBuffers(0) := pointer
    insideBoundsBuffers(0) := nextInsideBounds
  }

  io.offset := hitCounter
  io.availableSinks.valid := 0.B
  (io.availableSinks.data, sinks, insideBounds)
    .zipped
    .foreach { case (port, sink, insideBounds) =>
      port := Valid(insideBounds, sink)
    }
  io.addresses := addresses


  switch(stateReg) {
    is(State.Wait) {
      stateReg := Mux(io.changeSource.valid, State.ReceiveFirst, State.Wait)
    }
    is(State.ReceiveFirst) {
      stateReg := State.ReceiveSecond
      bufferLoad(0) := 1.B
      nextPointer := pointer + 1.U
    }
    is(State.ReceiveSecond) {
      stateReg := State.NormalOperation
      bufferLoad(1) := 1.B
      nextPointer := pointer + 1.U
      bufferSelect := startPointer(0)
      hitCounter := startPointer.takeLsb(log2Ceil(k))
    }
    is(State.NormalOperation) {
      io.availableSinks.valid := 1.B
      stateReg := Mux(io.changeSource.valid, State.ReceiveFirst, State.NormalOperation)
      when(doBufferSwitch) {
        bufferSelect := ~bufferSelect
        bufferLoad(bufferSelect) := 1.B
        nextPointer := pointer + 1.U
      }
    }
  }

  when(io.changeSource.valid) {
    val start = startPointerTable.read(io.changeSource.data.id).head
    nextPointer := start.dropLsb(log2Ceil(k))
    endPointer := endPointerTable.read(io.changeSource.data.id).head
    startPointer := start
  }

}

object AvailableSinks {

  object State extends ChiselEnum {
    val Wait, ReceiveFirst, ReceiveSecond, NormalOperation = Value
  }


  def bufferSelectionMask(k: Int): Vec[Vec[Bool]] = VecInit.tabulate(k) { i =>
    shifted(Seq.fill(k)(0), Seq.fill(k)(1), i).take(k).map(_.B).toVec
  }

  def inversion(b: Bool)(v: Vec[Bool]): Vec[Bool] = v.map(_ ^ b).toVec

  def main(args: Array[String]) = emitVerilog(new AvailableSinks(16384, 4, Graph.fromFile("src/graphs/d-d-64-v.csv", 16.W))(false), Array("--target-dir","build"))

  def apply(size: Int, k: Int, graph: Graph)(changeSource: Valid[Graph.Node], hits: UInt)(implicit simulation: Boolean): (Valid[Vec[Valid[Graph.Node]]], Vec[UInt], UInt) = {
    val availableSinks = Module(new AvailableSinks(size, k, graph))
    availableSinks.io.expand(
      _.changeSource := changeSource,
      _.hits := hits
    )
    (availableSinks.io.availableSinks, availableSinks.io.addresses, availableSinks.io.offset)
  }

}
