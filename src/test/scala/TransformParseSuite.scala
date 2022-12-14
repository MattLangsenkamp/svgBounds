import cats.data.NonEmptyList
import com.dprl.model.Transformation.*
import com.dprl.*
import cats.parse.{Parser, Parser0}

class TransformParseSuite extends munit.FunSuite {
  def testTransform[A <: Transformation](path: String, outCommand: A, parser: Parser[A])(implicit loc: munit.Location): Unit = {
    test("transform") {
      parser.parse(path) match
        case Right((_, res)) =>
          assertEquals(res, outCommand)
        case Left(_) =>
          assert(false)
    }
  }

  def testTransformList[A <: Transformation](path: String, outCommand: NonEmptyList[A], parser: Parser[NonEmptyList[A]])(implicit loc: munit.Location): Unit = {
    test("transform") {
      parser.parse(path) match
        case Right((_, res)) =>
          assertEquals(res, outCommand)
        case Left(_) =>
          assert(false)
    }
  }

  List(
    ("translate(1,1)", Translate(1, Some(1))),
    ("translate  (   1,1)   ", Translate(1, Some(1))),
    ("translate( 1 20 )", Translate(1, Some(20))),
    ("translate( 1)", Translate(1, None)),
    ("translate( 1 )", Translate(1, None)),
    ("translate(     1     0    )", Translate(1, Some(0)))
  ).foreach(testTransform(_, _, TransformParse.translate))

  List(
    ("scale(1,1)", Scale(1, Some(1))),
    ("scale  (   1,1)   ", Scale(1, Some(1))),
    ("scale( 1 20 )", Scale(1, Some(20))),
    ("scale( 1)", Scale(1, None)),
    ("scale( 1 )", Scale(1, None)),
    ("scale(     1     0       )", Scale(1, Some(0)))
  ).foreach(testTransform(_, _, TransformParse.scale))

  List(
    ("rotate(1,1,1)", Rotate(1, Some((1, 1)))),
    ("rotate  (   1,1,1)   ", Rotate(1, Some((1, 1)))),
    ("rotate( 1 20 333 )", Rotate(1, Some((20, 333)))),
    ("rotate( 1)", Rotate(1, None)),
    ("rotate( 1 )", Rotate(1, None)),
    ("rotate(     1     0   9     )", Rotate(1, Some((0, 9))))
  ).foreach(testTransform(_, _, TransformParse.rotate))

  List(
    ("skewX(1)", SkewX(1)),
    ("skewX  (   1)   ", SkewX(1)),
    ("skewX( 1  )", SkewX(1)),
    ("skewX(1 )", SkewX(1)),
  ).foreach(testTransform(_, _, TransformParse.skewX))

  List(
    ("skewY(1)", SkewY(1)),
    ("skewY  (   1)   ", SkewY(1)),
    ("skewY( 1  )", SkewY(1)),
    ("skewY(1 )", SkewY(1)),
  ).foreach(testTransform(_, _, TransformParse.skewY))

  List(
    ("matrix(1,1,1,1,1,1)", Matrix(1,1,1,1,1,1)),
    ("matrix  (   1,1,1,1,    1 1)   ", Matrix(1,1,1,1,1,1)),
    ("matrix( 1 20 20 30 44 145 )", Matrix(1,20,20,30,44,145))
  ).foreach(testTransform(_, _, TransformParse.matrix))

  List(
    ("matrix(1,1,1,1,1,1) matrix(1,1,1,1,1,1)", NonEmptyList.of(Matrix(1,1,1,1,1,1), Matrix(1,1,1,1,1,1))),
    ("matrix(1,1,1,1,1,1) skewY( 1  ) matrix(1,1,1,1,1,1)", NonEmptyList.of(Matrix(1,1,1,1,1,1), SkewY(1), Matrix(1,1,1,1,1,1))),
    ("     matrix(1,1,1,1,1,1) skewY( 1  ) matrix(1,1,1,1,1,1)     ", NonEmptyList.of(Matrix(1,1,1,1,1,1), SkewY(1), Matrix(1,1,1,1,1,1))),
    ("     matrix(1,1,1,1,1,1) skewY( 1  ) matrix(1,1,1,1,1,1) rotate( 1 ) rotate( 1 2, 2 )    ",
      NonEmptyList.of(Matrix(1,1,1,1,1,1), SkewY(1), Matrix(1,1,1,1,1,1), Rotate(1, None), Rotate(1, Some((2,2))))),
  ).foreach(testTransformList(_, _, TransformParse.transformList))
}
