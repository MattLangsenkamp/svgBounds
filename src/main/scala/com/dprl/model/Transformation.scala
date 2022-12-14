package com.dprl.model

import com.dprl.model.SvgType.Point

import scala.annotation.targetName
import scala.math.{cos, sin, tan}

object Transformation {
  trait Transformation {
    def toMatrix: Matrix
  }

  case class Matrix(a: Double, b: Double, c: Double, d: Double, e: Double, f: Double) extends Transformation {
    override def toMatrix: Matrix = this

    @targetName("matMul")
    def *(other: Matrix): Matrix = Matrix(
      this.a * other.a + this.c * other.b,          this.b * other.a + this.d * other.b,
      this.a * other.c + this.c + other.d,          this.b * other.c + this.d * other.d,
      this.a * other.e + this.c * other.f + this.e, this.b * other.e + this.d * other.f + this.f)

    @targetName("matMul")
    def *(p: Point): Point =
      Point(this.a * p.x + this.c * p.y + this.e, this.b * p.x + this.d * p.y + + this.f)

  }

  case class Translate(x: Double, y: Option[Double]) extends Transformation {
    override def toMatrix: Matrix = y match
      case Some(yReal) => Matrix(1, 0, 0, 1, x, yReal)
      case None => Matrix(1, 0, 0, 1, x, 0)
  }

  case class Scale(x: Double, y: Option[Double]) extends Transformation {
    override def toMatrix: Matrix = y match
      case Some(yReal) => Matrix(x, 0, 0, yReal, 0, 0)
      case None => Matrix(x, 0, 0, 1, 0, 0)
  }

  case class Rotate(a: Double, p: Option[(Double, Double)]) extends Transformation {
    override def toMatrix: Matrix =
      val radians = a.toRadians
      Matrix(cos(radians), sin(radians), -sin(radians), cos(radians), 0, 0)
  }

  case class SkewX(a: Double) extends Transformation {
    override def toMatrix: Matrix =
      val radians = a.toRadians
      Matrix(1, 0, tan(radians), 1, 0, 0)
  }

  case class SkewY(a: Double) extends Transformation {
    override def toMatrix: Matrix =
      val radians = a.toRadians
      Matrix(1, tan(radians), 0, 1, 0, 0)
  }
}
