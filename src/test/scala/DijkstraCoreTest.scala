
import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec



class DijkstraCoreTest extends AnyFlatSpec with ChiselScalatestTester{

  "DijkstraCore" should "find shortest paths" in {
    test(new DijkstraCore(4, Graph.fromFile("src/graphs/simple.csv", 16.W))(true))
      .withAnnotations(Seq(WriteVcdAnnotation)) { dut =>

        dut.clock.step()


        for(i <- 0 to 8) {
          dut.io.route.valid.poke(1.B)
          dut.io.route.bits.start.id.poke(0.U)
          dut.io.route.bits.end.id.poke(i.U)

          dut.clock.step()

          while (!dut.io.distance.valid.peek.litToBoolean) dut.clock.step()

          println(dut.io.distance.data.peek.litValue)
        }






      }
  }

}
