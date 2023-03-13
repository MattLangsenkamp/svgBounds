package org.dprl.svgbounds.model

import org.dprl.svgbounds.model.SvgType.Point
import Transformation.Matrix


// derived from https://svgwg.org/svg2-draft/paths.html#PathElement
object SvgCommand {
  sealed trait SvgCommand {
    def transform(matrix: Matrix): SvgCommand
  }

  case class EmptyCommand() extends SvgCommand {
    override def transform(matrix: Matrix): EmptyCommand = EmptyCommand()
  }

  // moveto commands
  case class M(p: Point) extends SvgCommand {
    override def transform(matrix: Matrix): M = M(matrix * p)
  }

  case class m_(p: Point) extends SvgCommand {
    override def transform(matrix: Matrix): m_ = m_(matrix * p)
  }

  // close path commands both do same thing
  case class Z() extends SvgCommand {
    override def transform(matrix: Matrix): Z = Z()
  }

  case class z_() extends SvgCommand {
    override def transform(matrix: Matrix): z_ = z_()
  }

  // lineto commands
  case class L(p: Point) extends SvgCommand {
    override def transform(matrix: Matrix): L = L(matrix * p)
  }

  case class l_(p: Point) extends SvgCommand {
    override def transform(matrix: Matrix): l_ = l_(matrix * p)
  }

  case class H(x: Double) extends SvgCommand {
    override def transform(matrix: Matrix): H = H((matrix * Point(x, 0)).x)
  }

  case class h_(x: Double) extends SvgCommand {
    override def transform(matrix: Matrix): h_ = h_((matrix * Point(x, 0)).x)
  }

  case class V(y: Double) extends SvgCommand {
    override def transform(matrix: Matrix): V = V((matrix * Point(0, y)).y)
  }

  case class v_(y: Double) extends SvgCommand {
    override def transform(matrix: Matrix): v_ = v_((matrix * Point(0, y)).y)
  }

  // cubic bezier commands
  case class C(control1: Point, control2: Point, dest: Point) extends SvgCommand {
    override def transform(matrix: Matrix): C =
      C(matrix * control1, matrix * control2, matrix * dest)
  }

  case class c_(control1: Point, control2: Point, dest: Point) extends SvgCommand {
    override def transform(matrix: Matrix): c_ =
      c_(matrix * control1, matrix * control2, matrix * dest)
  }

  case class S(control2: Point, dest: Point) extends SvgCommand {
    override def transform(matrix: Matrix): S =
      S(matrix * control2, matrix * dest)
  }

  case class s_(control2: Point, dest: Point) extends SvgCommand {
    override def transform(matrix: Matrix): s_ =
      s_(matrix * control2, matrix * dest)
  }

  // quadratic bezier curve commands
  case class Q(control1: Point, dest: Point) extends SvgCommand {
    override def transform(matrix: Matrix): Q =
      Q(matrix * control1, matrix * dest)
  }

  case class q_(control1: Point, dest: Point) extends SvgCommand {
    override def transform(matrix: Matrix): q_ =
      q_(matrix * control1, matrix * dest)
  }

  case class T(p: Point) extends SvgCommand {
    override def transform(matrix: Matrix): T = T(matrix * p)
  }

  case class t_(p: Point) extends SvgCommand {
    override def transform(matrix: Matrix): t_ = t_(matrix * p)
  }

  // elliptical arc
  case class A(rx: Float, ry: Float, xAxisRotation: Float, largeArcFlag: Short, sweepFlag: Short, dest: Point)
    extends SvgCommand {
    override def transform(matrix: Matrix): A = A(rx, ry, xAxisRotation, largeArcFlag, sweepFlag, matrix*dest)
  }

  case class a_(rx: Float, ry: Float, xAxisRotation: Float, largeArcFlag: Short, sweepFlag: Short, dest: Point)
    extends SvgCommand {
    override def transform(matrix: Matrix): a_ = a_(rx, ry, xAxisRotation, largeArcFlag, sweepFlag, matrix*dest)
  }

}
