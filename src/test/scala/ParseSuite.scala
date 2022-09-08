import cats.data.NonEmptyList
import com.dprl.*
import cats.parse.{Parser, Parser0}

class ParseSuite extends munit.FunSuite {

  def testCommand[A <: SvgCommand](path: String, outCommand: NonEmptyList[A], parser: Parser[NonEmptyList[A]])(implicit loc: munit.Location): Unit = {
    test("moveTo") {
      parser.parse(path) match
        case Right((_, res)) =>
          print(res)
          assertEquals(res, outCommand)
        case Left(_) =>
          assert(false)
    }
  }

  // MoveTo tests
  List(
    ("M 1 2", NonEmptyList[M](M(1, 2), List())),
    ("M, 1 2", NonEmptyList[M](M(1, 2), List())),
    ("M, 1, 2", NonEmptyList[M](M(1, 2), List())),
    ("M, 1,   2", NonEmptyList[M](M(1, 2), List())),
    ("M 1, 2 ", NonEmptyList[M](M(1, 2), List())),
    ("M 1, 2 15 255", NonEmptyList[M](M(1, 2), List(M(15, 255)))),
    ("M 1.4, 2 15 255", NonEmptyList[M](M(1.4, 2), List(M(15, 255)))),
  ).foreach(testCommand(_, _, Parse.movetoM))

  List(
    ("m 1 2", NonEmptyList[m_](m_(1, 2), List())),
    ("m, 1 2", NonEmptyList[m_](m_(1, 2), List())),
    ("m, 1, 2", NonEmptyList[m_](m_(1, 2), List())),
    ("m, 1,   2", NonEmptyList[m_](m_(1, 2), List())),
    ("m 1, 2 ", NonEmptyList[m_](m_(1, 2), List())),
    ("m 1, 2 15 255", NonEmptyList[m_](m_(1, 2), List(m_(15, 255)))),
    ("m 1.4, 2 15 255", NonEmptyList[m_](m_(1.4, 2), List(m_(15, 255)))),
  ).foreach(testCommand(_, _, Parse.movetoM_))

  // ClosePath tests

  // LineToTests
  List(
    ("L 1 2", NonEmptyList[L](L(1, 2), List())),
    ("L, 1 2", NonEmptyList[L](L(1, 2), List())),
    ("L, 1, 2", NonEmptyList[L](L(1, 2), List())),
    ("L, 1,   2", NonEmptyList[L](L(1, 2), List())),
    ("L 1, 2 ", NonEmptyList[L](L(1, 2), List())),
    ("L 1, 2 15 255", NonEmptyList[L](L(1, 2), List(L(15, 255)))),
    ("L 1.4, 2 15 255", NonEmptyList[L](L(1.4, 2), List(L(15, 255)))),
  ).foreach(testCommand(_, _, Parse.linetoL))

  List(
    ("l 1 2", NonEmptyList[l_](l_(1, 2), List())),
    ("l, 1 2", NonEmptyList[l_](l_(1, 2), List())),
    ("l, 1, 2", NonEmptyList[l_](l_(1, 2), List())),
    ("l, 1,   2", NonEmptyList[l_](l_(1, 2), List())),
    ("l 1, 2 ", NonEmptyList[l_](l_(1, 2), List())),
    ("l 1, 2 15 255", NonEmptyList[l_](l_(1, 2), List(l_(15, 255)))),
    ("l 1.4, 2 15 255", NonEmptyList[l_](l_(1.4, 2), List(l_(15, 255)))),
  ).foreach(testCommand(_, _, Parse.linetoL_))

  // Horizontal LineTo tests
  List(
    ("H 1", NonEmptyList[H](H(1), List())),
    ("H, 1", NonEmptyList[H](H(1), List())),
    ("H, 1, ", NonEmptyList[H](H(1), List())),
    ("H, 1,   ", NonEmptyList[H](H(1), List())),
    ("H 1, 2 ", NonEmptyList[H](H(1), List(H(2)))),
    ("H 1, 2 15 255", NonEmptyList[H](H(1), List(H(2), H(15), H(255)))),
    ("H 1.4, 2 15 255", NonEmptyList[H](H(1.4), List(H(2), H(15), H(255)))),
  ).foreach(testCommand(_, _, Parse.horizontalLinetoH))

  List(
    ("h 1", NonEmptyList[H](H(1), List())),
    ("h, 1", NonEmptyList[H](H(1), List())),
    ("h, 1, ", NonEmptyList[h_](H(1), List())),
    ("h, 1,   ", NonEmptyList[h_](H(1), List())),
    ("h 1, 2 ", NonEmptyList[h_](H(1), List(H(2)))),
    ("h 1, 2 15 255", NonEmptyList[h_](H(1), List(H(2), H(15), H(255)))),
    ("h 1.4, 2 15 255", NonEmptyList[h_](H(1.4), List(H(2), H(15), H(255)))),
  ).foreach(testCommand(_, _, Parse.horizontalLinetoH_))
  // Vertical LineTo tests
}