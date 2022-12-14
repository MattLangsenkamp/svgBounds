package com.dprl.model

import cats.data.NonEmptyList
import com.dprl.model.SvgCommand.SvgCommand
import com.dprl.model.Transformation.*

import scala.annotation.targetName
import scala.math.{cos, max, min, sin, tan}

object SvgType {

  case class Bounds(xMin: Double, yMin: Double, xMax: Double, yMax: Double) {
    def toRectTag: String = s"<rect x=\"$xMin\" y=\"$yMin\" width=\"${xMax - xMin}\" " +
      s"height=\"${yMax - yMin}\" fill=\"transparent\" stroke=\"black\"/>"

    @targetName("plus")
    def + (other: Bounds): Bounds =
      Bounds(min(this.xMin, other.xMin), min(this.yMin, other.yMin), max(this.xMax, other.xMax), max(this.yMax, other.yMax))

    @targetName("plus")
    def +(other: Point): Bounds =
      Bounds(min(this.xMin, other.x), min(this.yMin, other.y), max(this.xMax, other.x), max(this.yMax, other.y))
  }

  sealed trait SvgType {
    def toTag: String
  }

  case class Path(d: NonEmptyList[SvgCommand]) {
    @targetName("transform")
    def *(m: Matrix): Path = Path(d.map{
      svgCommand => svgCommand.transform(m)
    })
  }

  case class Rect() extends SvgType {
    override def toTag: String = ""
  }

  case class Point(x: Double, y: Double) {
    
    @targetName("plus")
    def +(other: Point): Point = Point(x+other.x, y+other.y)
  }

}
