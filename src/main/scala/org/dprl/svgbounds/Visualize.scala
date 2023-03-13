package org.dprl.svgbounds

import org.dprl.svgbounds.model.SvgType.Bounds

import scala.xml.*
object Visualize {

  def addBoundingBoxes(svg: Elem, boundingBoxes: List[Bounds]): Node =
    val root = if (svg.label == "svg") svg else svg \ "svg"
    boundingBoxes.foldLeft(root.head)((curSvg, b) => curSvg match
      case elem: Elem => elem.copy(child = elem.child :+ XML.loadString(b.toRectTag))
      case Group(nodes) => assert(false)
      case node: SpecialNode => assert(false)
      case _ => assert(false))

}
