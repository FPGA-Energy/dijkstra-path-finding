
import Helper.{BundleExpander, SeqDataExtension, SyncReadMemExtension, nextPow2}
import chisel3._
import chisel3.experimental.BundleLiterals.AddBundleLiteralConstructor
import chisel3.experimental.VecLiterals._
import chisel3.experimental.{ChiselAnnotation, annotate}
import chisel3.internal.firrtl.Width
import chisel3.util.log2Ceil
import chiseltest._
import firrtl.annotations.MemoryArrayInitAnnotation

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
    def apply(n: Int, w: Width, id: Int, weight: Int): OutGoingEdge = OutGoingEdge(n,w).Lit(_.end -> Node(n, id), _.weight -> weight.U(w))
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
    println(n)

    Graph(n, w, Seq.tabulate(n) { id =>
      Node(n, id) -> edges
        .filter(_._1 == id)
        .sortBy(_._2)
        .map { case (_, end, weight) =>
          OutGoingEdge(n, w, end, weight)
        }
    })
  }

}

class Graph(val n: Int, val w: Width, adjacencySeq: Seq[(Graph.Node,Seq[Graph.OutGoingEdge])]) {

  def generatePointerTable(implicit simulation: Boolean, clock: Clock): (ReadOnlyTable[UInt], ReadOnlyTable[UInt]) = {
    val (end, table) = adjacencySeq.foldLeft(0 -> Seq[(Int, Int)]()) { case ((top, table), (_, edges)) =>
      val m = edges.length
      (top + m, table :+ (top -> (top + m - 1)))
    }
    ReadOnlyTable.Asynchronous(1, table.map(_._1.U), Some("StartPointerTable")) -> ReadOnlyTable.Asynchronous(1, table.map(_._2.U), Some("EndPointerTable"))
  }

  def apply(node: Graph.Node): Seq[Graph.OutGoingEdge] = adjacencySeq.find(_._1 == node).get._2

  def apply(connection: (Graph.Node, Graph.Node)): Option[UInt] = {
    apply(connection._1).find(_.end == connection._2) match {
      case Some(edge) => Some(edge.weight)
      case None => None
    }
  }

  def adjacencyMatrix: (Seq[Seq[Bool]], Seq[Seq[UInt]]) = {
    Seq.tabulate(n,n) { (i,j) =>
      apply(Graph.Node(n, i) -> Graph.Node(n, j)) match {
        case Some(_) => 1.B
        case None => 0.B
      }
    } -> Seq.tabulate(n, n) { (i, j) =>
      apply(Graph.Node(n, i) -> Graph.Node(n, j)) match {
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
