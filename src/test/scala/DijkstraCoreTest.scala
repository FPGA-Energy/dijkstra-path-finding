
import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec



class DijkstraCoreTest extends AnyFlatSpec with ChiselScalatestTester{

  "DijkstraCore" should "find shortest paths" in {
    val g = Graph.fromFile("src/graphs/simple.csv", 16.W)
    test(new DijkstraCore(8, g)(true))
      .withAnnotations(Seq(WriteVcdAnnotation)) { dut =>

        dut.clock.step()

        for(i <- 0 to 15) {
          for(j <- 0 to 15) {
            dut.io.route.valid.poke(1.B)
            dut.io.route.bits.start.id.poke(i.U)
            dut.io.route.bits.end.id.poke(j.U)

            dut.clock.step()

            while (!dut.io.distance.valid.peek.litToBoolean) dut.clock.step()

            println(s"$i -> $j: ${dut.io.distance.data.peek.litValue}")
            g.distance(Graph.Node(g.n, i) -> Graph.Node(g.n, j)) match {
              case None =>
              case Some(d) => dut.io.distance.data.expect(d)
            }

          }
        }

      }
  }

  "DijkstraCore" should "find shortest paths d-d-64" in {
    val g = Graph.fromFile("src/graphs/d-d-64-v.csv", 17.W)
    test(new DijkstraCore(16, g)(true))
      //.withAnnotations(Seq(WriteVcdAnnotation))
      { dut =>

        dut.clock.step()
        dut.clock.setTimeout(0)

        for (i <- 1 until 64) {
          for (j <- 0 until 64) {
            dut.io.route.valid.poke(1.B)
            dut.io.route.bits.start.id.poke(i.U)
            dut.io.route.bits.end.id.poke(j.U)

            dut.clock.step()

            while (!dut.io.distance.valid.peek.litToBoolean) dut.clock.step()

            println(s"$i -> $j: ${dut.io.distance.data.peek.litValue}")
            g.distance(Graph.Node(g.n, i) -> Graph.Node(g.n, j)) match {
              case None =>
              case Some(d) => dut.io.distance.data.expect(d)
            }

          }
        }

      }
  }

  "DijkstraCore" should "find shortest paths d-ud-64" in {
    val g = Graph.fromFile("src/graphs/d-ud-64-v.csv", 17.W)
    test(new DijkstraCore(4, g)(true))
    //.withAnnotations(Seq(WriteVcdAnnotation))
    { dut =>

      dut.clock.step()
      dut.clock.setTimeout(0)

      for (i <- 1 until 64) {
        for (j <- 0 until 64) {
          dut.io.route.valid.poke(1.B)
          dut.io.route.bits.start.id.poke(i.U)
          dut.io.route.bits.end.id.poke(j.U)

          dut.clock.step()

          while (!dut.io.distance.valid.peek.litToBoolean) dut.clock.step()

          println(s"$i -> $j: ${dut.io.distance.data.peek.litValue}")
          g.distance(Graph.Node(g.n, i) -> Graph.Node(g.n, j)) match {
            case None =>
            case Some(d) => dut.io.distance.data.expect(d)
          }

        }
      }

    }
  }

  "DijkstraCore" should "find shortest paths s-d-64" in {
    val g = Graph.fromFile("src/graphs/s-d-64-v.csv", 19.W)
    test(new DijkstraCore(4, g)(true))
    //.withAnnotations(Seq(WriteVcdAnnotation))
    { dut =>

      dut.clock.step()
      dut.clock.setTimeout(0)

      for (i <- 1 until 64) {
        for (j <- 0 until 64) {
          dut.io.route.valid.poke(1.B)
          dut.io.route.bits.start.id.poke(i.U)
          dut.io.route.bits.end.id.poke(j.U)

          dut.clock.step()

          while (!dut.io.distance.valid.peek.litToBoolean) dut.clock.step()

          println(s"$i -> $j: ${dut.io.distance.data.peek.litValue}")
          g.distance(Graph.Node(g.n, i) -> Graph.Node(g.n, j)) match {
            case None =>
            case Some(d) => dut.io.distance.data.expect(d)
          }

        }
      }

    }
  }

  "DijkstraCore" should "find shortest paths s-ud-64" in {
    val g = Graph.fromFile("src/graphs/s-ud-64-v.csv", 18.W)
    test(new DijkstraCore(4, g)(true))
    //.withAnnotations(Seq(WriteVcdAnnotation))
    { dut =>

      dut.clock.step()
      dut.clock.setTimeout(0)

      for (i <- 1 until 64) {
        for (j <- 0 until 64) {
          dut.io.route.valid.poke(1.B)
          dut.io.route.bits.start.id.poke(i.U)
          dut.io.route.bits.end.id.poke(j.U)

          dut.clock.step()

          while (!dut.io.distance.valid.peek.litToBoolean) dut.clock.step()

          println(s"$i -> $j: ${dut.io.distance.data.peek.litValue}")
          g.distance(Graph.Node(g.n, i) -> Graph.Node(g.n, j)) match {
            case None =>
            case Some(d) => dut.io.distance.data.expect(d)
          }

        }
      }

    }
  }

}
