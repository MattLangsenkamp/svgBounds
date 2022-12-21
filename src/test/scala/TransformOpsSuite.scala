import cats.data.NonEmptyList
import com.dprl.model.Transformation.*
import com.dprl.*
import cats.parse.{Parser, Parser0}
import scala.math.abs

class TransformOpsSuite extends munit.FunSuite {

  def testTransformCollapse(trans: String, outMatrix: Matrix)(implicit loc: munit.Location): Unit = {
    test("transformCollapse") {
      TransformParse.transformList.parse(trans) match
        case Left(_) =>
          assert(false)
        case Right(value) =>
          assert(fuzzyMatComp(BoundOps.CollapseTransforms(value._2), outMatrix))
    }
  }

  List(
    ("matrix(1,0,0,1,0,0) matrix(1,0,0,1,0,0)", Matrix(1, 0, 0, 1, 0, 0)),
    ("matrix(1,0,0,1,0,0) matrix(1,1,1,1,1,1)", Matrix(1, 1, 1, 1, 1, 1)),
    ("matrix(1,0,0,1,0,0) translate(1,1)", Matrix(1, 0, 0, 1, 1, 1)),
    ("matrix(1,0,0,1,0,0) translate(8,0)", Matrix(1, 0, 0, 1, 8, 0)),
    ("matrix(1,0,0,1,0,0) scale(8,0)", Matrix(8, 0, 0, 0, 0, 0)),
    ("matrix(1,0,0,1,0,0) scale(8) scale(1,7)", Matrix(8, 0, 0, 56, 0, 0)),
    ("matrix(1,0,0,1,0,0) scale(8) scale(1,7) scale(2)", Matrix(16, 0, 0, 112, 0, 0)),
    ("matrix(1,0,0,1,0,0) rotate(-45)", Matrix(.7071067811865476, -.7071067811865475, .7071067811865475, .7071067811865476, 0, 0)),
    ("translate(4, -3) rotate(90)", Matrix(0, 1, -1, 0, 3, 4)),

  ).foreach(testTransformCollapse(_, _))

  def fuzzyMatComp(out: Matrix, truth: Matrix): Boolean =
    fuzzyComp(out.a, truth.a) && fuzzyComp(out.b, truth.b)
      && fuzzyComp(out.c, truth.c) && fuzzyComp(out.d, truth.d)
      && fuzzyComp(out.e, truth.e) && fuzzyComp(out.f, truth.f)

  def fuzzyComp(d1: Double, d2: Double): Boolean = abs(d1 - d2) < 0.00000001
}
