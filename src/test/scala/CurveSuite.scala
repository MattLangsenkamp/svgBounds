import cats.data.NonEmptyList
import org.dprl.svgbounds.model.SvgCommand.*
import org.dprl.svgbounds.*
import org.dprl.svgbounds.model.SvgType.{Bounds, Point}
import cats.parse.{Parser, Parser0}
import org.dprl.svgbounds.CurveUtils

class CurveSuite extends munit.FunSuite {
  def testQuadBezier(bounds: Bounds, start: Point, com: Q | q_ )(implicit loc: munit.Location): Unit = {
    test("QuadraticBezierCurve") {
      com match
        case c: Q  =>
          assert(CurveUtils.quadraticBezierBoundingBox(start, c.control1, c.dest).equals(bounds))
        case c: q_ =>
          val adjustedControl = start + c.control1
          val adjustedDest = start + c.dest
          assert(CurveUtils.quadraticBezierBoundingBox(start, adjustedControl, adjustedDest).equals(bounds))
    }
  }

  List[(Bounds, Point, Q | q_)](
    (Bounds(10.0,80.0,60.0,120.0), Point(10, 80), q_(Point(20, 30) ,Point(50, 40))),
    (Bounds(5.555555555555555,71.0,60.0,120.0), Point(10, 80), q_(Point(-20, -30) ,Point(50, 40))),
    (Bounds(10.0,80.0,64.44444444444444,120.0), Point(10, 80), q_(Point(70, 30) ,Point(50, 40))),
    (Bounds(10.0,80.0,60,137.85714285714283), Point(10, 80), q_(Point(20, 90) ,Point(50, 40))),
    (Bounds(10.0,43.18181818181817,60,120), Point(10, 80), q_(Point(20, -90) ,Point(50, 40))),
    (Bounds(10.0,38.333333333333336,50.0,80), Point(10, 80), Q(Point(20, 30) ,Point(50, 40))),
    (Bounds(0.9999999999999991,12.777777777777777,50.0,80), Point(10, 80), Q(Point(-20, -30) ,Point(50, 40))),
    (Bounds(-50, 12.777777777777777, 10.0,80), Point(10, 80), Q(Point(-20, -30) ,Point(-50, 40))),
    (Bounds(20.0,17.5,60.0,30.0), Point(20, 30), Q(Point(40,5), Point(60,30)))
  ).foreach(testQuadBezier)

  def testCubicBezier(bounds: Bounds, start: Point, com: C | c_)(implicit loc: munit.Location): Unit = {
    test("CubicBezierCurve") {
      com match
        case c: C =>
          assert(CurveUtils.cubicBezierBoundingBox(start, c.control1, c.control2, c.dest).equals(bounds))
        case c: c_ =>
          val adjustedControl1 = start + c.control1
          val adjustedControl2 = start + c.control2
          val adjustedDest = start + c.dest
          assert(
            CurveUtils.cubicBezierBoundingBox(start, adjustedControl1, adjustedControl2, adjustedDest)
              .equals(bounds))
    }
  }

  List[(Bounds, Point, C | c_)](
    (Bounds(10.0,80.0,60.0,120.0), Point(10, 80), c_(Point(20, 30), Point(20, 30) ,Point(50, 40))),
    (Bounds(-40.0,80.0,21.506562731868144,120.0), Point(10, 80), c_(Point(20, 30), Point(20, 30) ,Point(-50, 40))),
    (Bounds(-40.0,40.0,21.506562731868144,99.04261199259082), Point(10, 80), c_(Point(20, 30), Point(20, 30) ,Point(-50, -40))),
    (Bounds(9.999999999999998,80.0,25.0,165.0), Point(10, 80), c_(Point(20, 30), Point(-20, -30), Point(15, 85))),
    (Bounds(10.0,80.0,36.666666666666664,165.0), Point(10, 80), c_(Point(20, 30), Point(40, -30), Point(15, 85))),
    (Bounds(10.0,80.0,42.86141631955827,133.07622383768168), Point(10, 80), c_(Point(20, 30), Point(60, 100), Point(5, 5))),
    (Bounds(5.0,5.0,33.416499049802084,80.0), Point(10, 80), C(Point(20, 30), Point(60, 100), Point(5, 5)))

  ).foreach(testCubicBezier)

  def testEllipticalArc(bounds: Bounds, start: Point, com: A | a_)(implicit loc: munit.Location): Unit = {
    test("EllipticalArcCurve") {
      com match
        case c: A =>
          assert(CurveUtils.ellipticalArcBoundingBox(
            start, c.rx, c.ry, c.xAxisRotation, c.largeArcFlag, c.sweepFlag, c.dest).equals(bounds))
        case c: a_ =>
          val adjustedDest = start + c.dest
          assert(CurveUtils.ellipticalArcBoundingBox(
          start, c.rx, c.ry, c.xAxisRotation, c.largeArcFlag, c.sweepFlag, adjustedDest)
          .equals(bounds))
    }
  }
  // this is still broken :(
  List[(Bounds, Point, A | a_)](
    //(Bounds(17.049150281252622,19.549150281252626,70.0,60.0), 20, 60, a_(25,25, -30, 0,1, 50,-25)),
    //(Bounds(17.049150281252622,35.0,72.95084971874738,75.45084971874738), 20, 60, a_(25,25, -30, 0,0, 50,-25)),
    //(Bounds(20.0,15.532961156792624,82.75834048869649,60.0), 20, 60, a_(4,25, 50, 0, 1, 50, -25)),
  ).foreach(testEllipticalArc)
}


