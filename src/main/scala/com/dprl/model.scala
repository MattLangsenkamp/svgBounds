package com.dprl

import cats.data.NonEmptyList

import scala.math.{cos, max, min, sin, tan}
import scala.annotation.targetName

// ------------ model for components that make up a path ------------

// derived from https://svgwg.org/svg2-draft/paths.html#PathElement
trait SvgCommand

case class EmptyCommand() extends SvgCommand

// moveto commands
case class M(p: Point) extends SvgCommand // absolute

case class m_(p: Point) extends SvgCommand // relative

// close path commands both do same thing
case class Z() extends SvgCommand

case class z_() extends SvgCommand

// lineto commands
case class L(p: Point) extends SvgCommand // absolute

case class l_(p: Point) extends SvgCommand // relative

case class H(x: Double) extends SvgCommand // absolute

case class h_(x: Double) extends SvgCommand // relative

case class V(y: Double) extends SvgCommand // absolute

case class v_(y: Double) extends SvgCommand // relative

// cubic bezier commands
case class C(control1: Point, control2: Point, dest: Point) extends SvgCommand // absolute

case class c_(control1: Point, control2: Point, dest: Point) extends SvgCommand // relative

case class S(control2: Point, dest: Point) extends SvgCommand // absolute

case class s_(control2: Point, dest: Point) extends SvgCommand // relative

// quadratic bezier curve commands
case class Q(control1: Point, dest: Point) extends SvgCommand // absolute

case class q_(control1: Point, dest: Point) extends SvgCommand // relative

case class T(p: Point) extends SvgCommand // absolute

case class t_(p: Point) extends SvgCommand // relative

// elliptical arc
case class A(rx: Float, ry: Float, xAxisRotation: Float, largeArcFlag: Short, sweepFlag: Short, dest: Point)
  extends SvgCommand

case class a_(rx: Float, ry: Float, xAxisRotation: Float, largeArcFlag: Short, sweepFlag: Short, dest: Point)
  extends SvgCommand

// ------------ non inter-path model ------------

trait SvgType {
  def toTag: String
}

case class Bounds(xMin: Double, yMin: Double, xMax: Double, yMax: Double) {
  def toRectTag: String = s"<rect x=\"$xMin\" y=\"$yMin\" width=\"${xMax - xMin}\" " +
    s"height=\"${yMax - yMin}\" fill=\"transparent\" stroke=\"black\"/>"

  @targetName("plusEq")
  def +=(other: Bounds): Bounds =
    Bounds(min(this.xMin, other.xMin), min(this.yMin, other.yMin), max(this.xMax, other.xMax), max(this.yMax, other.yMax))
}

case class Point(x: Double, y: Double)

case class Path(d: NonEmptyList[SvgCommand]) {
  @targetName("transform")
  def *(m: Matrix): Path = ???
}

case class Rect()

// transform model
trait Transform {
  def toMatrix: Matrix
}

case class Matrix(a: Double, b: Double, c: Double, d: Double, e: Double, f: Double) extends Transform {
  override def toMatrix: Matrix = this

  @targetName("matMul")
  def *(other: Matrix): Matrix = Matrix(
    this.a * other.a + this.c * other.b,          this.b * other.a + this.d * other.b,
    this.a * other.c + this.c + other.d,          this.b * other.c + this.d * other.d,
    this.a * other.e + this.c * other.f + this.e, this.b * other.e + this.d * other.f + this.f)

  @targetName("matMul")
  def *(p: (Double, Double)): (Double, Double) =
    (this.a * p._1 + this.c * p._2 + this.e, this.b * p._1 + this.d * p._2 + + this.f)
}

case class Translate(x: Double, y: Option[Double]) extends Transform {
  override def toMatrix: Matrix = y match
    case Some(yReal) => Matrix(1, 0, 0, 1, x, yReal)
    case None => Matrix(1, 0, 0, 1, x, 0)
}

case class Scale(x: Double, y: Option[Double]) extends Transform {
  override def toMatrix: Matrix = y match
    case Some(yReal) => Matrix(x, 0, 0, yReal, 0, 0)
    case None => Matrix(x, 0, 0, 1, 0, 0)
}

case class Rotate(a: Double, p: Option[(Double, Double)]) extends Transform {
  override def toMatrix: Matrix =
    val radians = a.toRadians
    Matrix(cos(radians), sin(radians), -sin(radians), cos(radians), 0, 0)
}

case class SkewX(a: Double) extends Transform {
  override def toMatrix: Matrix =
    val radians = a.toRadians
    Matrix(1, 0, tan(radians), 1, 0, 0)
}

case class SkewY(a: Double) extends Transform {
  override def toMatrix: Matrix =
    val radians = a.toRadians
    Matrix(1, tan(radians), 0, 1, 0, 0)
}