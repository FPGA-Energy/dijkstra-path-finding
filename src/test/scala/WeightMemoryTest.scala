
import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec

class WeightMemoryTest extends AnyFlatSpec with ChiselScalatestTester {

  behavior of "WeightMemory"

  it should "provide weights to neighbors for each node in d-d-64-v.csv" in {

    val k = 4
    val g = Graph.fromFile("src/graphs/d-d-64-v.csv", 16.W)
    println(g(Graph.Node(g.n, 2)).mkString("\n"))
    test(new WeightMemory(16384, k, g)(true)).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>

      for (source <- g.nodes) {

        dut.io.changeSource.data.poke(source)
        dut.io.changeSource.valid.poke(1.B)

        dut.clock.step()

        dut.io.changeSource.valid.poke(0.B)

        for (group <- 0 until g.nodes.length / k) {

          dut.io.sinkGroup.valid.poke(1.B)
          dut.io.sinkGroup.data.poke(group.U)

          dut.clock.step()
          while (!dut.io.weights.valid.peek.litToBoolean) dut.clock.step()

          dut.io.sinkGroup.valid.poke(0.B)

          for (i <- 0 until k) {
            g(source -> Graph.Node(g.n, group * k + i)) match {
              case Some(weight) => {
                println(s"${source.id.litValue} -> ${group * k + i}: ${weight.litValue} == ${if (dut.io.weights.data(i).valid.peek.litToBoolean) dut.io.weights.data(i).data.peek.litValue else "None"}")
                dut.io.weights.data(i).valid.expect(1.B, s"error for Node(${group * k + i})")
                dut.io.weights.data(i).data.expect(weight)
              }
              case None => {
                println(s"${source.id.litValue} -> ${group * k + i}: None == ${if (dut.io.weights.data(i).valid.peek.litToBoolean) dut.io.weights.data(i).data.peek.litValue else "None"}")
                dut.io.weights.data(i).valid.expect(0.B, s"error for Node(${group * k + i})")
              }
            }

          }

        }

      }

    }
  }
    it should "provide weights to neighbors for each node in simple" in {

      val k = 4
      val g = Graph.fromFile("src/graphs/simple.csv", 16.W)
      println(g(Graph.Node(g.n, 2)).mkString("\n"))
      test(new WeightMemory(16384, k, g)(true)).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>

        for (source <- g.nodes) {

          dut.io.changeSource.data.poke(source)
          dut.io.changeSource.valid.poke(1.B)

          dut.clock.step()

          dut.io.changeSource.valid.poke(0.B)

          for (group <- 0 until g.nodes.length / k) {

            dut.io.sinkGroup.valid.poke(1.B)
            dut.io.sinkGroup.data.poke(group.U)

            dut.clock.step()
            while (!dut.io.weights.valid.peek.litToBoolean) dut.clock.step()

            dut.io.sinkGroup.valid.poke(0.B)

            for (i <- 0 until k) {
              g(source -> Graph.Node(g.n, group * k + i)) match {
                case Some(weight) => {
                  println(s"${source.id.litValue} -> ${group * k + i}: ${weight.litValue} == ${if (dut.io.weights.data(i).valid.peek.litToBoolean) dut.io.weights.data(i).data.peek.litValue else "None"}")
                  dut.io.weights.data(i).valid.expect(1.B, s"error for Node(${group * k + i})")
                  dut.io.weights.data(i).data.expect(weight)
                }
                case None => {
                  println(s"${source.id.litValue} -> ${group * k + i}: None == ${if (dut.io.weights.data(i).valid.peek.litToBoolean) dut.io.weights.data(i).data.peek.litValue else "None"}")
                  dut.io.weights.data(i).valid.expect(0.B, s"error for Node(${group * k + i})")
                }
              }

            }

          }

        }

      }

    }

}
