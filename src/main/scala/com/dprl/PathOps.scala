package com.dprl

import cats.data.NonEmptyList
import cats.parse.Parser
import com.dprl.model.SvgCommand.*
import com.dprl.model.Transformation.*
import com.dprl.model.SvgType.{Point, Path, Bounds}
import scala.annotation.targetName

object PathOps {

  def CollapseTransforms(transformList: NonEmptyList[Transformation]): Matrix =
  // initialize with identity matrix?
    transformList.foldLeft(Matrix(1, 0, 0, 1, 0, 0))(
      (curMatrix, transform) => transform.toMatrix * curMatrix
    )

  def parsePath(pathString: String): Either[Parser.Error, Path] = PathParse.svgPath.parse(pathString) match
    case Left(value) => Left(value)
    case Right(value) => Right(Path(value._2))

  def getBounds(path: Path): Bounds = path.d.foldLeft[(Point, SvgCommand, Bounds)](
    (
      Point(0,0),
      EmptyCommand(),
      Bounds(xMin = Double.MaxValue, yMin = Double.MaxValue, xMax = Double.MinValue, yMax = Double.MinValue)
    )
  ) {
    (state, com) =>
      val curPoint = state._1
      val prevCom = state._2
      val curBounds = state._3
      com match
        case e: EmptyCommand => (curPoint, e, curBounds)
        case M(p) => (p, com, curBounds + p)
        case m_(p) => (curPoint + p, com, curBounds + (curPoint + p))
        case Z() => (curPoint, com, Bounds)
        case z_() => (curPoint, com, Bounds)
        case L(p) => (p, com, curBounds + p)
        case l_(p) => (curPoint + p, com, curBounds + (curPoint + p))
        case H(x) => (p, com, curBounds + p)
        case h_(x) => (curPoint + p, com, curBounds + (curPoint + p))
        case V(y) => (p, com, curBounds + p)
        case v_(y) => (curPoint + p, com, curBounds + (curPoint + p))
        case C(control1, control2, dest) => ???
        case c_(control1, control2, dest) => ???
        case S(control2, dest) => ???
        case s_(control2, dest) => ???
        case Q(control1, dest) => ???
        case q_(control1, dest) => ???
        case T(p) => ???
        case t_(p) => ???
        case A(rx, ry, xAxisRotation, largeArcFlag, sweepFlag, dest) => ???
        case a_(rx, ry, xAxisRotation, largeArcFlag, sweepFlag, dest) => ???
  }._3

}
