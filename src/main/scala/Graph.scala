
import Helper.{BundleExpander, SeqDataExtension, SyncReadMemExtension, nextPow2}
import chisel3._
import chisel3.experimental.BundleLiterals.AddBundleLiteralConstructor
import chisel3.experimental.VecLiterals._
import chisel3.experimental.{ChiselAnnotation, annotate, prefix}
import chisel3.internal.firrtl.Width
import chisel3.util.log2Ceil
import chiseltest._
import firrtl.annotations.MemoryArrayInitAnnotation

import scala.collection.mutable
import scala.io.Source
import scala.math

object Graph {

  object Node {
    def apply(n: Int): Node = new Node(n)
    def apply(n: Int, id: Int): Node = Node(n).Lit(_.id -> id.U)
    def apply(n: Int, id: UInt): Node = Wire(Node(n)).expand(
      _.id := id
    )
  }
  class Node(val n: Int) extends Bundle {
    val id = UInt(log2Ceil(n).W)

    def =/=(that: Graph.Node) = id =/= that.id
    def ===(that: Graph.Node) = id === that.id
    override def litValue: BigInt = id.litValue
    def ==(that: Graph.Node) = this.id.litValue == that.id.litValue
    //override def toString(): String = s"Node(${id.litValue})"
  }

  object OutGoingEdge {
    def apply(n: Int, w: Width): OutGoingEdge = new OutGoingEdge(n, w)
    def apply(n: Int, w: Width, end: Graph.Node, weight: Int): OutGoingEdge = OutGoingEdge(n,w).Lit(_.end -> end, _.weight -> weight.U(w))
  }
  class OutGoingEdge(n: Int, w: Width) extends Bundle {
    val end = Node(n)
    val weight = UInt(w)

    override def toString(): String = s"OutGoingEdge($end, ${weight.litValue})"
    def bits: BigInt = (end.litValue << weight.getWidth) | weight.litValue
  }

  private def trim(s: String) = s.substring(1, s.length - 1)

  def apply(n: Int, w: Width, adjacencySeq:  Seq[(Graph.Node,Seq[Graph.OutGoingEdge])]): Graph = new Graph(n, w, adjacencySeq)

  def fromFile(fileName: String, w: Width): Graph = {
    val edges = Source.fromFile(fileName)
      .getLines()
      .map(_.split(","))
      .map { case Array(start, end, weight) =>
        (trim(start).toInt, trim(end).toInt, trim(weight).toInt)
      }
      .toSeq

    val n = nextPow2(edges.map(_._1).max + 1)

    Graph(n, w, Seq.tabulate(n) { id =>
      Node(n, id) -> edges
        .filter(_._1 == id)
        .sortBy(_._2)
        .map { case (_, end, weight) =>
          OutGoingEdge(n, w, Node(n, end), weight)
        }
    })
  }

}

class Graph(val n: Int, val w: Width, adjacencySeq: Seq[(Graph.Node,Seq[Graph.OutGoingEdge])]) {

  def generatePointerTable: (Seq[Int],Seq[Int]) = {
    val (end, table) = adjacencySeq.foldLeft(0 -> Seq[(Int, Int)]()) { case ((top, table), (_, edges)) =>
      val m = edges.length
      (top + m, table :+ (top -> (top + m - { if (m == 0) 0 else 1 })))
    }
    (table.map(_._1),table.map(_._2))
  }

  def generatePointerRoms(implicit simulation: Boolean, clock: Clock): (ReadOnlyTable[UInt], ReadOnlyTable[UInt]) = {
    val (startPointers, endPointers) = generatePointerTable
    ReadOnlyTable.Asynchronous(1, startPointers.map(_.U), Some("StartPointerTable")) -> ReadOnlyTable.Asynchronous(1, endPointers.map(_.U), Some("EndPointerTable"))
  }

  def apply(node: Graph.Node): Seq[Graph.OutGoingEdge] = adjacencySeq.find(_._1 == node).get._2

  def edge(connection: (Graph.Node, Graph.Node)): Option[UInt] = {
    apply(connection._1).find(_.end == connection._2) match {
      case Some(edge) => Some(edge.weight)
      case None => None
    }
  }


  def distance(route: (Graph.Node, Graph.Node)): Option[UInt] = {

    val (start, end) = route


    val previous = mutable.ArrayBuffer.fill[Option[Graph.Node]](n)(None)
    val dist = mutable.ArrayBuffer.fill[Option[BigInt]](n)(None)
    dist(start.id.litValue.toInt) = Some(0)



    val ordering: Ordering[(Int, BigInt)] = (a, b) => a._2.compareTo(b._2)
    val queue = mutable.PriorityQueue(start.id.litValue.toInt -> BigInt(0))(ordering)

    while(queue.nonEmpty) {

      val (current, distance) = queue.dequeue()
      val edges = adjacencySeq.find(_._1.id.litValue.toInt == current).get._2

      edges.foreach { e =>
        val newRoute = distance + e.weight.litValue
        dist(e.end.id.litValue.toInt) match {
          case None  =>
            dist(e.end.litValue.toInt) = Some(newRoute)
            previous(e.end.litValue.toInt) = Some(nodes(current))
            if (!queue.exists(_._1 == e.end.id.litValue.toInt)) queue.enqueue(e.end.id.litValue.toInt -> newRoute)
          case Some(oldRoute) if oldRoute > newRoute =>
            dist(e.end.litValue.toInt) = Some(newRoute)
            previous(e.end.litValue.toInt) = Some(nodes(current))
            if (!queue.exists(_._1 == e.end.id.litValue.toInt)) queue.enqueue(e.end.id.litValue.toInt -> newRoute)
          case _ =>
          }
        }
      }

    dist(end.id.litValue.toInt) match {
      case None => None
      case Some(v) => Some(v.U)
    }
  }


  def adjacencyMatrix: (Seq[Seq[Bool]], Seq[Seq[UInt]]) = {
    Seq.tabulate(n,n) { (i,j) =>
      edge(Graph.Node(n, i) -> Graph.Node(n, j)) match {
        case Some(_) => 1.B
        case None => 0.B
      }
    } -> Seq.tabulate(n, n) { (i, j) =>
      edge(Graph.Node(n, i) -> Graph.Node(n, j)) match {
        case Some(weight) => weight
        case None => 0.U(w)
      }
    }
  }

  def getSinkTable: Seq[Graph.Node] = adjacencySeq.flatMap(_._2).map(_.end)
  def getWeightTable: Seq[UInt] = adjacencySeq.flatMap(_._2).map(_.weight)

  def nodes: Seq[Graph.Node] = adjacencySeq.map(_._1)

  override def toString: String = adjacencySeq.map(a => a._1.toString() + "\n\t" + a._2.mkString("\n\t")).mkString("\n")

}


object Test extends App {

  val g = Graph.fromFile("src/graphs/d-d-64-v.csv", 16.W)

  g.nodes.foreach { end =>
    println(g.distance(g.nodes(0) -> end))
  }

}
