import cats.data.NonEmptyList
import com.dprl.*
import cats.parse.{Parser, Parser0}

class CurveSuite extends munit.FunSuite {
  def testQuadBezier(bounds: Bounds, startX: Double, startY: Double, com: Q | q_ | T | t_, preCom: Option[Q | q_] )(implicit loc: munit.Location): Unit = {
    test("QuadraticBezierCurve") {
      com match
        case c: Q  =>
          assert(CurveUtils.quadraticBezierBoundingBox(startX, startY, c.x1, c.y1, c.x, c.y).equals(bounds))
        case c: q_ =>
          assert(CurveUtils.quadraticBezierBoundingBox(startX, startY, startX+c.x1, startY+c.y1, startX+c.x, startY+c.y)
              .equals(bounds))
        case c: T => preCom match
          case Some(value) => value match
            case v: Q => ???
            case v: q_ => ???
          case _ => assert(false)
        case c: t_ => preCom match
          case Some(value) => value match
            case v: Q => ???
            case v: q_ => ???
          case _ => assert(false)
    }
  }

  List[(Bounds, Double, Double, Q | q_ | T | t_, Option[Q | q_])](
    (Bounds(10.0,80.0,60.0,120.0), 10, 80, q_(20, 30 ,50, 40), None),
    (Bounds(5.555555555555555,71.0,60.0,120.0), 10, 80, q_(-20, -30 ,50, 40), None),
    (Bounds(10.0,80.0,64.44444444444444,120.0), 10, 80, q_(70, 30 ,50, 40), None),
    (Bounds(10.0,80.0,60,137.85714285714283), 10, 80, q_(20, 90 ,50, 40), None),
    (Bounds(10.0,43.18181818181817,60,120), 10, 80, q_(20, -90 ,50, 40), None),
    (Bounds(10.0,38.333333333333336,50.0,80), 10, 80, Q(20, 30 ,50, 40), None),
    (Bounds(0.9999999999999991,12.777777777777777,50.0,80), 10, 80, Q(-20, -30 ,50, 40), None),
    (Bounds(-50, 12.777777777777777, 10.0,80), 10, 80, Q(-20, -30 ,-50, 40), None),
  ).foreach(testQuadBezier)

  def testCubicBezier(bounds: Bounds, startX: Double, startY: Double, com: C | c_)(implicit loc: munit.Location): Unit = {
    com match
      case c: C => ???
      case c: c_ => ???
  }
}
