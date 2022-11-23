import cats.data.NonEmptyList
import com.dprl.*
import cats.parse.{Parser, Parser0}

class CurveSuite extends munit.FunSuite {
  def testQuadBezier(bounds: (Double, Double, Double, Double), startX: Double, startY: Double, com: Q | q_ | T | t_)(implicit loc: munit.Location): Unit = {
    test("QuadraticBezierCurve") {
      com match
        case c: Q => CurveUtils.quadraticBezierBoundingBox(startX, startY, c.x1, c.y1, c.x, c.y).equals(bounds)
        case c: q_ => CurveUtils.quadraticBezierBoundingBox(startX, startY, startX+c.x1, startY+c.y1, startX+c.x, startY+c.y).equals(bounds)
        case c: T => ???
        case c: t_ => ???
    }
  }

  List(((0,0,0,0), 0, 0, Q))

  def testCubicBezier(bounds: (Double, Double, Double, Double), startX: Double, startY: Double, com: C | c_)(implicit loc: munit.Location): Unit = {
    com match
      case c: C => ???
      case c: c_ => ???
  }
}
