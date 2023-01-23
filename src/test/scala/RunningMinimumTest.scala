
import chisel3._
import chisel3.experimental.VecLiterals._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec
import Helper._

class RunningMinimumTest extends AnyFlatSpec with ChiselScalatestTester {

  "RunningMinimum" should "provide the minimum at any point" in {
    test(new RunningMinimum(1024, 4, 16.W)).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>



      for(i <- 0 until 30) {

        dut.io.candidates.foreach { candidate =>
          candidate.valid.poke(RandBool())
          candidate.data.distance.poke(RandUInt(0 until 20))
          candidate.data.node.id.poke(RandUInt(0 until 1024))
        }

        dut.clock.step()

      }

    }
  }

}
