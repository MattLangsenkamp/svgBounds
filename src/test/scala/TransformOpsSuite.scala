import cats.data.NonEmptyList
import com.dprl.model.Transformation.*
import com.dprl.*
import cats.parse.{Parser, Parser0}

class TransformOpsSuite extends munit.FunSuite {

  def testTransformCollapse(trans: String, outMatrix: Matrix)(implicit loc: munit.Location): Unit = {
    test("transformCollapse") {
      TransformParse.transformList.parse(trans) match
        case Left(_) =>
          assert(false)
        case Right(value) =>
          // println(value._2)
          // println(PathOps.CollapseTransforms(value._2))
          assert(PathOps.CollapseTransforms(value._2).equals(outMatrix))

    }
  }

  List(
    ("matrix(1,0,0,1,0,0) matrix(1,0,0,1,0,0)", Matrix(1, 0, 0, 1, 0, 0)),
    ("matrix(1,0,0,1,0,0) matrix(1,1,1,1,1,1)", Matrix(1, 1, 1, 1, 1, 1)),
    ("matrix(1,0,0,1,0,0) translate(1,1)", Matrix(1, 0, 0, 1, 1, 1)),
    ("matrix(1,0,0,1,0,0) translate(8,0)", Matrix(1, 0, 0, 1, 8, 0)),
  ).foreach(testTransformCollapse(_, _))

}
