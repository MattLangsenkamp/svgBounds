package com.dprl

import scala.math.{cos, sin, tan}
import scala.annotation.targetName

// ------------ model for components that make up a path ------------

// derived from https://svgwg.org/svg2-draft/paths.html#PathElement
trait SvgCommand

case class EmptyCommand() extends SvgCommand

// moveto commands
case class M(x: Float, y: Float) extends SvgCommand // absolute

case class m_(x: Float, y: Float) extends SvgCommand // relative

// close path commands both do same thing
case class Z() extends SvgCommand

case class z_() extends SvgCommand

// lineto commands
case class L(x: Float, y: Float) extends SvgCommand // absolute

case class l_(x: Float, y: Float) extends SvgCommand // relative

case class H(x: Float) extends SvgCommand // absolute

case class h_(x: Float) extends SvgCommand // relative

case class V(y: Float) extends SvgCommand // absolute

case class v_(y: Float) extends SvgCommand // relative

// cubic bezier commands
case class C(x1: Float, y1: Float, x2: Float, y2: Float, x: Float, y: Float) extends SvgCommand // absolute

case class c_(x1: Float, y1: Float, x2: Float, y2: Float, x: Float, y: Float) extends SvgCommand // relative

case class S(x2: Float, y2: Float, x: Float, y: Float) extends SvgCommand // absolute

case class s_(x2: Float, y2: Float, x: Float, y: Float) extends SvgCommand // relative

// quadratic bezier curve commands
case class Q(x1: Float, y1: Float, x: Float, y: Float) extends SvgCommand // absolute

case class q_(x1: Float, y1: Float, x: Float, y: Float) extends SvgCommand // relative

case class T(x: Float, y: Float) extends SvgCommand // absolute

case class t_(x: Float, y: Float) extends SvgCommand // relative

// elliptical arc
case class A(rx: Float, ry: Float, xAxisRotation: Float, largeArcFlag: Short, sweepFlag: Short, x: Float, y: Float)
  extends SvgCommand

case class a_(rx: Float, ry: Float, xAxisRotation: Float, largeArcFlag: Short, sweepFlag: Short, x: Float, y: Float)
  extends SvgCommand

// ------------ non inter-path model ------------

trait SvgType {
  def toTag: String
}

case class Bounds(xMin: Double, yMin: Double, xMax: Double, yMax: Double) {
  def toRectTag: String = s"<rect x=\"$xMin\" y=\"$yMin\" width=\"${xMax - xMin}\" " +
    s"height=\"${yMax - yMin}\" fill=\"transparent\" stroke=\"black\"/>"

  @targetName("plusEq")
  def +=(other: Bounds): Bounds = ???
}

case class Path(d: String) {
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