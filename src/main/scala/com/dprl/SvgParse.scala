package com.dprl
import cats.kernel.Monoid
import cats.parse.Parser
import com.dprl.model.Transformation.Matrix
import com.dprl.Main.time
import scala.xml.{Elem, Node, XML}
import com.dprl.model.SvgType.{Bounds, Path, Rect, newBounds}

import scala.util.Right
object SvgParse {

  def defaultParse(svg: String): (List[(Bounds, Char)], Bounds) =
    val root = XML.loadString(svg)
    parse[List[(Bounds, Char)]](root, List(), (l: List[(Bounds, Char)], b: Bounds, c: Char) => {
      l.::((b,c))
    })

  def parse[A](root : Elem, state: A, f: (A, Bounds, Char) => A): (A, Bounds) =
    val matrix = Matrix(1,0,0,1,0,0)
    if (root.label == "svg") root.foldLeft(state, newBounds)((l, n) => {
        val recurse = parseRecursive(n, matrix, state, f)
        (recurse._1, l._2 + recurse._2)
      })
    else (root \ "svg").foldLeft(state, newBounds)((l, n) => {
      val recurse = parseRecursive(n, matrix, state, f)
      (recurse._1, l._2 + recurse._2)
    })

  private def parseRecursive[A](node: Node, matrix: Matrix, state: A, f: (A, Bounds, Char) => A): (A, Bounds) = {
    val path = (node \ "path").foldLeft((state, newBounds))((l, n)=>{
      val label = Integer.parseInt(n.attributes.asAttrMap("data-c"), 16).toChar
      val nMatrix = potentiallyTransformMatrix(n, matrix)
      val nBounds = parsePath(n, nMatrix)
      val nState = f(l._1, nBounds, label)
      (nState, l._2+nBounds)
    })
    val g = (node \ "g").foldLeft((path._1, path._2))((l, n)=>{
      val nMatrix = potentiallyTransformMatrix(n, matrix)
      val recurse = parseRecursive(n, nMatrix, l._1, f)
      (recurse._1 , l._2 + recurse._2)
    })
    (node \ "rect").foldLeft((g._1, g._2))((l, n)=>{
      val nMatrix = potentiallyTransformMatrix(n, matrix)
      val nBounds = parseRect(n, nMatrix)
      val nState = f(l._1, nBounds, '-')
      (nState, l._2 + nBounds)
    })
    (node \ "circle").foldLeft((g._1, g._2))((l, n) => {
      val nMatrix = potentiallyTransformMatrix(n, matrix)
      val nBounds = parseRect(n, nMatrix)
      val nState = f(l._1, nBounds, 'o')
      (nState, l._2 + nBounds)
    })
  }

  private def potentiallyTransformMatrix(node: Node, matrix: Matrix): Matrix = {
      node.attributes.asAttrMap.get("transform") match
        case Some(transform) => TransformParse.transformList.parse(transform) match
          case Left(_) =>
            println("failed to parse transform matrix")
            matrix
          case Right(value) =>
            matrix * BoundOps.CollapseTransforms(value._2)
        case None => matrix
  }

  def parsePath(path: Node, matrix: Matrix): Bounds =
    PathParse.svgPath.parse(path.attributes.asAttrMap("d")) match
      case Left(_) =>
        println("error parsing path")
        assert(false)
      case Right(value) =>
        BoundOps.getBounds(Path(value._2) * matrix)


  def parseRect(node: Node, matrix: Matrix): Bounds =
    val m = node.attributes.asAttrMap
    val rect = Rect(m("x").toDouble, m("y").toDouble, m("width").toDouble, m("height").toDouble)
    val nRect = rect * matrix
    Bounds(nRect.x, nRect.y, nRect.x + nRect.width, nRect.y + nRect.height)

  // def parseCircle(node: Node, matrix: Matrix): Bounds =
  //   val m = node.attributes.asAttrMap
  //  val circle = Circle(m("cx").toDouble, m("cy").toDouble, c("r").toDouble)
}
