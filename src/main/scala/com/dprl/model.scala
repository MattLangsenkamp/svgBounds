package com.dprl

import cats.data.NonEmptyList
import com.dprl.model.SvgCommand.*
import scala.math.{cos, max, min, sin, tan}
import scala.annotation.targetName




case class Bounds(xMin: Double, yMin: Double, xMax: Double, yMax: Double) {
  def toRectTag: String = s"<rect x=\"$xMin\" y=\"$yMin\" width=\"${xMax - xMin}\" " +
    s"height=\"${yMax - yMin}\" fill=\"transparent\" stroke=\"black\"/>"

  @targetName("plusEq")
  def +=(other: Bounds): Bounds =
    Bounds(min(this.xMin, other.xMin), min(this.yMin, other.yMin), max(this.xMax, other.xMax), max(this.yMax, other.yMax))
}



