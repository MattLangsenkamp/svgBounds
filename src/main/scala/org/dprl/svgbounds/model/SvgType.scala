package org.dprl.svgbounds.model

import cats.data.NonEmptyList
import org.dprl.svgbounds.model.SvgCommand.SvgCommand
import org.dprl.svgbounds.model.Transformation.*
import org.dprl.svgbounds.model.SvgCommand.SvgCommand
import org.dprl.svgbounds.model.Transformation.Matrix

import scala.annotation.targetName
import scala.math.{cos, max, min, sin, tan}

object SvgType {

  case class Bounds(xMin: Double, yMin: Double, xMax: Double, yMax: Double) {
    def toRectTag: String = Rect(x=xMin, y=yMin, width= xMax - xMin, height = yMax - yMin).toTag

    @targetName("plus")
    def +(other: Bounds): Bounds =
      Bounds(min(this.xMin, other.xMin), min(this.yMin, other.yMin), max(this.xMax, other.xMax), max(this.yMax, other.yMax))

    @targetName("plus")
    def +(other: Point): Bounds =
      Bounds(min(this.xMin, other.x), min(this.yMin, other.y), max(this.xMax, other.x), max(this.yMax, other.y))
  }

  def newBounds: Bounds = Bounds(Double.MaxValue, Double.MaxValue, Double.MinValue, Double.MinValue)

  sealed trait SvgType {
    def toTag: String

    @targetName("transform")
    def *(m: Matrix): SvgType
  }

  case class Path(d: NonEmptyList[SvgCommand]) {
    @targetName("transform")
    def *(m: Matrix): Path = Path(d.map {
      svgCommand => svgCommand.transform(m)
    })
  }

  case class Rect(x: Double, y: Double, width: Double, height: Double) extends SvgType {
    override def toTag: String =
        <rect y={y.toString} x={x.toString} width={width.toString} height={height.toString} style="fill:blue;stroke:pink;stroke-width:5;fill-opacity:0.1;stroke-opacity:0.9"/>.toString()

    @targetName("transform")
    override def *(m: Matrix): Rect =
      val p1 = m * Point(x, y)
      val p2 = m * Point(x+width, y)
      val p3 = m * Point(x, y+height)
      val p4 = m * Point(x+width, y+height)
      val minX = List(p1.x, p2.x, p3.x, p4.x).min
      val minY = List(p1.y, p2.y, p3.y, p4.y).min
      val maxX = List(p1.x, p2.x, p3.x, p4.x).max
      val maxY = List(p1.y, p2.y, p3.y, p4.y).max
      Rect(minX, minY, maxX-minX, maxY-minY)
  }

  case class Point(x: Double, y: Double) {

    @targetName("plus")
    def +(other: Point): Point = Point(x + other.x, y + other.y)

    def projectOver(other: Point): Point = Point(other.x-math.abs(x - other.x), other.y-math.abs(y - other.y))
  }

  // case class Circle(cx: Double, cy: Double, r: Double) extends SvgType {
  //  @targetName("transform")
  //  override def *(m: Matrix): Rect = ???
  // }

}
