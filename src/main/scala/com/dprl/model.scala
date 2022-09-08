package com.dprl


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

case class A(rx: Float, ry: Float, xAxisRotation: Float, largeArcFlag: Short, sweepFlag: Short, x: Float, y: Float)
  extends SvgCommand

case class a_(rx: Float, ry: Float, xAxisRotation: Float, largeArcFlag: Short, sweepFlag: Short, x: Float, y: Float)
  extends SvgCommand