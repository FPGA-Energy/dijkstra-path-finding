
import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec

class AvailableSinksTest extends AnyFlatSpec with ChiselScalatestTester {

  behavior of "AvailableSinks"

  it should "show available sinks" in {
    val g = Graph.fromFile("src/graphs/d-d-64-v.csv", 16.W)
    test(new AvailableSinks(4096, 4, g)(true)).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>

      dut.io.changeSource.valid.poke(1.B)
      dut.io.changeSource.data.poke(Graph.Node(g.n, 0))
      dut.clock.step()

      dut.io.changeSource.valid.poke(0.B)


      dut.clock.step(10)

      dut.io.hits.poke(1.U)
      dut.clock.step()
      dut.io.hits.poke(2.U)
      dut.clock.step()
      dut.io.hits.poke(3.U)
      dut.clock.step()
      dut.io.hits.poke(4.U)
      dut.clock.step()

      dut.io.changeSource.valid.poke(1.B)
      dut.io.changeSource.data.poke(Graph.Node(g.n, 1))
      dut.clock.step()

      dut.io.changeSource.valid.poke(0.B)

      dut.clock.step(10)

      dut.io.hits.poke(1.U)
      dut.clock.step()
      dut.io.hits.poke(2.U)
      dut.clock.step()
      dut.io.hits.poke(3.U)
      dut.clock.step()
      dut.io.hits.poke(4.U)
      dut.clock.step()
    }
  }

}
