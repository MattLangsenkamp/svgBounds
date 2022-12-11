package com.dprl
import scala.xml.{Elem, Node}

object SvgParse {

  case class bbox(x: Float, y: Float, width: Float, height: Float, char: Char)

  def parse(root : Elem): List[bbox] =
    (root \ "svg").foldLeft(List[bbox]())((l, n) => {
      println(n.label)
      val matrix = Matrix(1,1,1,1,1,1)
      l ::: parseRecursive(n, matrix)
    })

  def parseRecursive(node: Node, matrix: Matrix): List[bbox] = {
    (node \ "path").foldLeft(List[bbox]())((l, n)=>{
      val label = Integer.parseInt(n.attributes.asAttrMap("data-c"), 16).toChar
      val path = n.attributes.asAttrMap("path")
      val nMatrix = potentiallyTransformMatrix(n, matrix)
      l
    }) ::: (node \ "g").foldLeft(List[bbox]())((l, n)=>{
      val nMatrix = potentiallyTransformMatrix(n, matrix)
      l ::: parseRecursive(n, nMatrix)
    }) ::: (node \ "rect").foldLeft(List[bbox]())((l, n)=>{
      val nMatrix = potentiallyTransformMatrix(n, matrix)
      l
    })

  }

  def potentiallyTransformMatrix( node: Node, matrix: Matrix): Matrix = ???

}
