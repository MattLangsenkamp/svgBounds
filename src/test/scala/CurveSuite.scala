import cats.data.NonEmptyList
import com.dprl.*
import cats.parse.{Parser, Parser0}

class CurveSuite extends munit.FunSuite {
  def testQuadBezier(bounds: Bounds, startX: Double, startY: Double, com: Q | q_ )(implicit loc: munit.Location): Unit = {
    test("QuadraticBezierCurve") {
      com match
        case c: Q  =>
          assert(CurveUtils.quadraticBezierBoundingBox(startX, startY, c.x1, c.y1, c.x, c.y).equals(bounds))
        case c: q_ =>
          assert(CurveUtils.quadraticBezierBoundingBox(startX, startY, startX+c.x1, startY+c.y1, startX+c.x, startY+c.y)
              .equals(bounds))
    }
  }

  List[(Bounds, Double, Double, Q | q_)](
    (Bounds(10.0,80.0,60.0,120.0), 10, 80, q_(20, 30 ,50, 40)),
    (Bounds(5.555555555555555,71.0,60.0,120.0), 10, 80, q_(-20, -30 ,50, 40)),
    (Bounds(10.0,80.0,64.44444444444444,120.0), 10, 80, q_(70, 30 ,50, 40)),
    (Bounds(10.0,80.0,60,137.85714285714283), 10, 80, q_(20, 90 ,50, 40)),
    (Bounds(10.0,43.18181818181817,60,120), 10, 80, q_(20, -90 ,50, 40)),
    (Bounds(10.0,38.333333333333336,50.0,80), 10, 80, Q(20, 30 ,50, 40)),
    (Bounds(0.9999999999999991,12.777777777777777,50.0,80), 10, 80, Q(-20, -30 ,50, 40)),
    (Bounds(-50, 12.777777777777777, 10.0,80), 10, 80, Q(-20, -30 ,-50, 40)),
    (Bounds(20.0,17.5,60.0,30.0), 20, 30, Q(40,5, 60,30))
  ).foreach(testQuadBezier)

  def testCubicBezier(bounds: Bounds, startX: Double, startY: Double, com: C | c_)(implicit loc: munit.Location): Unit = {
    test("CubicBezierCurve") {
      com match
        case c: C =>
          assert(CurveUtils.cubicBezierBoundingBox(startX, startY, c.x1, c.y1, c.x2, c.y2, c.x, c.y).equals(bounds))
        case c: c_ =>

          assert(
            CurveUtils.cubicBezierBoundingBox(
              startX, startY, startX + c.x1, startY + c.y1, startX + c.x2, startY + c.y2, startX + c.x, startY + c.y)
              .equals(bounds))
    }
  }

  List[(Bounds, Double, Double, C | c_)](
    (Bounds(10.0,80.0,60.0,120.0), 10, 80, c_(20, 30, 20, 30 ,50, 40)),
    (Bounds(-40.0,80.0,21.506562731868144,120.0), 10, 80, c_(20, 30, 20, 30 ,-50, 40)),
    (Bounds(-40.0,40.0,21.506562731868144,99.04261199259082), 10, 80, c_(20, 30, 20, 30 ,-50, -40)),
    (Bounds(9.999999999999998,80.0,25.0,165.0), 10, 80, c_(20, 30, -20, -30, 15, 85)),
    (Bounds(10.0,80.0,36.666666666666664,165.0), 10, 80, c_(20, 30, 40, -30, 15, 85)),
    (Bounds(10.0,80.0,42.86141631955827,133.07622383768168), 10, 80, c_(20, 30, 60, 100, 5, 5)),
    (Bounds(5.0,5.0,33.416499049802084,80.0), 10, 80, C(20, 30, 60, 100, 5, 5))

  ).foreach(testCubicBezier)

  def testEllipticalArc(bounds: Bounds, startX: Double, startY: Double, com: A | a_)(implicit loc: munit.Location): Unit = {
    test("EllipticalArcCurve") {
      com match
        case c: A =>
          println(CurveUtils.ellipticalArcBoundingBox(
            startX, startY, c.rx, c.ry, c.xAxisRotation, c.largeArcFlag, c.sweepFlag, c.x, c.y).toRectTag)
          println(CurveUtils.ellipticalArcBoundingBox(
            startX, startY, c.rx, c.ry, c.xAxisRotation, c.largeArcFlag, c.sweepFlag, c.x, c.y))
          assert(CurveUtils.ellipticalArcBoundingBox(
            startX, startY, c.rx, c.ry, c.xAxisRotation, c.largeArcFlag, c.sweepFlag, c.x, c.y).equals(bounds))
        case c: a_ =>
          println(CurveUtils.ellipticalArcBoundingBox(
            startX, startY, c.rx, c.ry, c.xAxisRotation, c.largeArcFlag, c.sweepFlag, startX + c.x, startY + c.y).toRectTag)
          println(CurveUtils.ellipticalArcBoundingBox(
            startX, startY, c.rx, c.ry, c.xAxisRotation, c.largeArcFlag, c.sweepFlag, startX + c.x, startY + c.y))
          assert(CurveUtils.ellipticalArcBoundingBox(
          startX, startY, c.rx, c.ry, c.xAxisRotation, c.largeArcFlag, c.sweepFlag, startX + c.x, startY + c.y)
          .equals(bounds))
    }
  }

  List[(Bounds, Double, Double, A | a_)](
    (Bounds(10.0,80.0,60.0,120.0), 20, 60, a_(25,25, -30, 0,1, 50,-25)),
  ).foreach(testEllipticalArc)
}


