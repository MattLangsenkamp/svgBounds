package org.dprl.svgbounds

import cats.data.NonEmptyList
import cats.parse.Parser
import org.dprl.svgbounds.CurveUtils
import org.dprl.svgbounds.model.SvgType.{Bounds, Path, Point}
import org.dprl.svgbounds.model.Transformation.*
import org.dprl.svgbounds.model.SvgCommand.*
import org.dprl.svgbounds.CurveUtils.*

import scala.annotation.targetName
import scala.xml.Elem

object BoundOps {

  def CollapseTransforms(transformList: NonEmptyList[Transformation]): Matrix =
  // initialize with identity matrix?
    transformList.reverse.foldLeft(Matrix(1, 0, 0, 1, 0, 0))(
      (curMatrix, transform) => transform.toMatrix * curMatrix
    )

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
      // anytime we get a relative command we set the previous
      // command to the absolute version of that same command to
      // simplify the shorthand curve calculations
      com match
        case e: EmptyCommand => (curPoint, e, curBounds)
        case M(p) => (p, com, curBounds + p)
        case m_(p) => (curPoint + p, com, curBounds + (curPoint + p))
        case Z() => (curPoint, com, curBounds)
        case z_() => (curPoint, com, curBounds)
        case L(p) => (p, com, curBounds + p)
        case l_(p) => (curPoint + p, com, curBounds + (curPoint + p))
        case H(x) => (Point(x, curPoint.y), com, curBounds + Point(x, curPoint.y))
        case h_(x) => (curPoint + Point(x, 0), com, curBounds + Point(x + curPoint.x, curPoint.y))
        case V(y) => (Point(curPoint.x, y), com, curBounds + Point(curPoint.x, y))
        case v_(y) => (curPoint + Point(0, y), com, curBounds + Point(curPoint.x, y + curPoint.y))
        case C(control1, control2, dest) =>
          (dest, com, curBounds + cubicBezierBoundingBox(curPoint, control1, control2, dest))
        case c_(control1, control2, dest) =>
          val adjustedControl1 = curPoint + control1
          val adjustedControl2 = curPoint+control2
          val adjustedDest = curPoint+dest
          (curPoint + dest, C(adjustedControl1, adjustedControl2, adjustedDest),
            curBounds + cubicBezierBoundingBox(curPoint, adjustedControl1, adjustedControl2, adjustedDest))
        case c: S => handleS(curPoint, prevCom, curBounds, c)
        case c: s_ => handleS_(curPoint, prevCom, curBounds, c)
        case Q(control1, dest) =>
          (dest, com, curBounds + quadraticBezierBoundingBox(curPoint, control1, dest))
        case q_(control1, dest) =>
          val adjustedControl1 = curPoint + control1
          val adjustedDest = curPoint+dest
          (curPoint + dest, Q(adjustedControl1, adjustedDest),
            curBounds + quadraticBezierBoundingBox(curPoint, adjustedControl1, adjustedDest))
        case c: T => handleT(curPoint, prevCom, curBounds, c)
        case c: t_ => handleT_(curPoint, prevCom, curBounds, c)
        case A(rx, ry, xAxisRotation, largeArcFlag, sweepFlag, dest) =>
          (dest, com,
            curBounds + ellipticalArcBoundingBox(curPoint, rx, ry, xAxisRotation, largeArcFlag, sweepFlag, dest))
        case a_(rx, ry, xAxisRotation, largeArcFlag, sweepFlag, dest) =>
          val adjustedDest = curPoint+dest
          (dest, A(rx, ry, xAxisRotation, largeArcFlag, sweepFlag, adjustedDest),
          curBounds + ellipticalArcBoundingBox(curPoint, rx, ry, xAxisRotation, largeArcFlag, sweepFlag, adjustedDest))
  }._3

  private def handleS(curPoint: Point, prevCom: SvgCommand, curBounds: Bounds, s: S): (Point, SvgCommand, Bounds) =
    prevCom match
      case S(lastControl, _) =>
        (s.dest, s, curBounds + CurveUtils.cubicBezierBoundingBox(curPoint, lastControl projectOver curPoint, s.control2, s.dest))
      case C(_, lastControl, _) =>
        (s.dest, s, curBounds + CurveUtils.cubicBezierBoundingBox(curPoint, lastControl projectOver curPoint, s.control2, s.dest))
      case _ => (s.dest, s, curBounds + CurveUtils.cubicBezierBoundingBox(curPoint, curPoint, s.control2, s.dest))
  private def handleS_(curPoint: Point, prevCom: SvgCommand, curBounds: Bounds, s: s_): (Point, SvgCommand, Bounds) = {
    val adjustedControl2 = curPoint + s.control2
    val adjustedDest = curPoint + s.dest
    prevCom match
      case S(lastControl, _) =>
        (s.dest, S(adjustedControl2, adjustedDest), curBounds +
          CurveUtils.cubicBezierBoundingBox(
            curPoint, lastControl projectOver curPoint, adjustedControl2, adjustedDest))
      case C(_, lastControl, _) =>
        (s.dest, S(adjustedControl2, adjustedDest), curBounds +
          CurveUtils.cubicBezierBoundingBox(
            curPoint, lastControl projectOver curPoint, adjustedControl2, adjustedDest))
      case _ =>
        (s.dest, S(adjustedControl2, adjustedDest), curBounds +
          CurveUtils.cubicBezierBoundingBox(curPoint, curPoint, adjustedControl2, adjustedDest))
  }

  // all T, t_ become Q in previous command
  private def handleT(curPoint: Point, prevCom: SvgCommand, curBounds: Bounds, t: T): (Point, SvgCommand, Bounds) =
    prevCom match
      case Q(lastControl, _) => (t.p, Q(lastControl projectOver curPoint, t.p),
        curBounds + CurveUtils.quadraticBezierBoundingBox(curPoint, lastControl projectOver curPoint, t.p))
      case _ => (t.p, Q(curPoint, t.p),
        curBounds + CurveUtils.quadraticBezierBoundingBox(curPoint, curPoint, t.p))
  private def handleT_(curPoint: Point, prevCom: SvgCommand, curBounds: Bounds, t: t_): (Point, SvgCommand, Bounds) =
    val adjustedDest = t.p + curPoint
    prevCom match
      case Q(lastControl, _) => (adjustedDest, Q(lastControl projectOver curPoint, adjustedDest),
        curBounds + CurveUtils.quadraticBezierBoundingBox(curPoint, lastControl projectOver curPoint, adjustedDest))
      case _ => (adjustedDest, Q(curPoint, adjustedDest),
        curBounds + CurveUtils.quadraticBezierBoundingBox(curPoint, curPoint, adjustedDest))
}
