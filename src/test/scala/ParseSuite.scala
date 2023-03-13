import cats.data.NonEmptyList
import org.dprl.svgbounds.*
import org.dprl.svgbounds.model.SvgType.Point
import cats.parse.{Parser, Parser0}
import org.dprl.svgbounds.PathParse
import org.dprl.svgbounds.model.SvgCommand.*

class ParseSuite extends munit.FunSuite {

  def testCommand[A <: SvgCommand](path: String, outCommand: NonEmptyList[A], parser: Parser[NonEmptyList[A]])(implicit loc: munit.Location): Unit = {
    test("moveTo") {
      parser.parse(path) match
        case Right((_, res)) =>
          assertEquals(res, outCommand)
        case Left(_) =>
          assert(false)
    }
  }

  def testCommand0[A <: SvgCommand](path: String, outCommand: NonEmptyList[A], parser: Parser0[NonEmptyList[A]])(implicit loc: munit.Location): Unit = {
    test("moveTo") {
      parser.parse(path) match
        case Right((_, res)) =>
          assertEquals(res, outCommand)
        case Left(_) =>
          assert(false)
    }
  }

  // MoveTo tests
  List(
    ("M 1 2", NonEmptyList[M](M(Point(1, 2)), List())),
    ("M 1 2", NonEmptyList[M](M(Point(1, 2)), List())),
    ("M 1, 2", NonEmptyList[M](M(Point(1, 2)), List())),
    ("M 1,   2", NonEmptyList[M](M(Point(1, 2)), List())),
    ("M 1, 2 ", NonEmptyList[M](M(Point(1, 2)), List())),
    ("M1, 2 15 255", NonEmptyList[M](M(Point(1, 2)), List(M(Point(15, 255))))),
    ("M 1.4, 2 15 255", NonEmptyList[M](M(Point(1.4, 2)), List(M(Point(15, 255))))),
  ).foreach(testCommand(_, _, PathParse.movetoM))

  List(
    ("m 1 2", NonEmptyList[m_](m_(Point(1, 2)), List())),
    ("m 1 2", NonEmptyList[m_](m_(Point(1, 2)), List())),
    ("m 1, 2", NonEmptyList[m_](m_(Point(1, 2)), List())),
    ("m 1,   2", NonEmptyList[m_](m_(Point(1, 2)), List())),
    ("m1, 2 ", NonEmptyList[m_](m_(Point(1, 2)), List())),
    ("m 1, 2 15 255", NonEmptyList[m_](m_(Point(1, 2)), List(m_(Point(15, 255))))),
    ("m 1.4, 2 15 255", NonEmptyList[m_](m_(Point(1.4, 2)), List(m_(Point(15, 255))))),
  ).foreach(testCommand(_, _, PathParse.movetoM_))

  // ClosePath tests

  // LineToTests
  List(
    ("L 1 2", NonEmptyList[L](L(Point(1, 2)), List())),
    ("L1 2", NonEmptyList[L](L(Point(1, 2)), List())),
    ("L1, 2", NonEmptyList[L](L(Point(1, 2)), List())),
    ("L 1,   2", NonEmptyList[L](L(Point(1, 2)), List())),
    ("L 1, 2 ", NonEmptyList[L](L(Point(1, 2)), List())),
    ("L1, 2 15 255", NonEmptyList[L](L(Point(1, 2)), List(L(Point(15, 255))))),
    ("L 1.4, 2 15 255", NonEmptyList[L](L(Point(1.4, 2)), List(L(Point(15, 255))))),
  ).foreach(testCommand(_, _, PathParse.linetoL))

  List(
    ("l 1 2", NonEmptyList[l_](l_(Point(1, 2)), List())),
    ("l 1 2", NonEmptyList[l_](l_(Point(1, 2)), List())),
    ("l1, 2", NonEmptyList[l_](l_(Point(1, 2)), List())),
    ("l 1,   2", NonEmptyList[l_](l_(Point(1, 2)), List())),
    ("l 1, 2 ", NonEmptyList[l_](l_(Point(1, 2)), List())),
    ("l 1, 2 15 255", NonEmptyList[l_](l_(Point(1, 2)), List(l_(Point(15, 255))))),
    ("l 1.4, 2 15 255", NonEmptyList[l_](l_(Point(1.4, 2)), List(l_(Point(15, 255))))),
  ).foreach(testCommand(_, _, PathParse.linetoL_))

  // Horizontal LineTo tests
  List(
    ("H 1", NonEmptyList[H](H(1), List())),
    ("H 1", NonEmptyList[H](H(1), List())),
    ("H 1, ", NonEmptyList[H](H(1), List())),
    ("H1,   ", NonEmptyList[H](H(1), List())),
    ("H 1, 2 ", NonEmptyList[H](H(1), List(H(2)))),
    ("H1, 2 15 255", NonEmptyList[H](H(1), List(H(2), H(15), H(255)))),
    ("H 1.4, 2 15 255", NonEmptyList[H](H(1.4), List(H(2), H(15), H(255)))),
  ).foreach(testCommand(_, _, PathParse.horizontalLinetoH))

  List(
    ("h 1", NonEmptyList[h_](h_(1), List())),
    ("h 1", NonEmptyList[h_](h_(1), List())),
    ("h 1, ", NonEmptyList[h_](h_(1), List())),
    ("h 1,   ", NonEmptyList[h_](h_(1), List())),
    ("h1, 2 ", NonEmptyList[h_](h_(1), List(h_(2)))),
    ("h 1, 2 15 255", NonEmptyList[h_](h_(1), List(h_(2), h_(15), h_(255)))),
    ("h 1.4, 2 15 255", NonEmptyList[h_](h_(1.4), List(h_(2), h_(15), h_(255)))),
  ).foreach(testCommand(_, _, PathParse.horizontalLinetoH_))

  // Vertical LineTo tests
  List(
    ("V 1", NonEmptyList[V](V(1), List())),
    ("V1", NonEmptyList[V](V(1), List())),
    ("V 1, ", NonEmptyList[V](V(1), List())),
    ("V 1,   ", NonEmptyList[V](V(1), List())),
    ("V1, 2 ", NonEmptyList[V](V(1), List(V(2)))),
    ("V 1, 2 15 255", NonEmptyList[V](V(1), List(V(2), V(15), V(255)))),
    ("V 1.4, 2 15 255", NonEmptyList[V](V(1.4), List(V(2), V(15), V(255)))),
  ).foreach(testCommand(_, _, PathParse.verticalLinetoV))

  List(
    ("v 1", NonEmptyList[v_](v_(1), List())),
    ("v1", NonEmptyList[v_](v_(1), List())),
    ("v 1, ", NonEmptyList[v_](v_(1), List())),
    ("v 1,   ", NonEmptyList[v_](v_(1), List())),
    ("v 1, 2 ", NonEmptyList[v_](v_(1), List(v_(2)))),
    ("v 1, 2 15 255", NonEmptyList[v_](v_(1), List(v_(2), v_(15), v_(255)))),
    ("v 1.4, 2 15 255", NonEmptyList[v_](v_(1.4), List(v_(2), v_(15), v_(255)))),
  ).foreach(testCommand(_, _, PathParse.verticalLinetoV_))

  // curveto commands
  List(
    ("C 1 1 2 2 3 3", NonEmptyList[C](C(Point(1,1),Point(2,2),Point(3,3)), List())),
    ("C 1  1  2  2 3 3", NonEmptyList[C](C(Point(1,1),Point(2,2),Point(3,3)), List())),
    ("C 1, 1 2, 2 3, 3", NonEmptyList[C](C(Point(1,1),Point(2,2),Point(3,3)), List())),
    ("C 1 1 .2 2.55 3 3", NonEmptyList[C](C(Point(1,1),Point(.2,2.55),Point(3,3)), List())),
    ("C1 1 2 2 3 ,3 ", NonEmptyList[C](C(Point(1,1),Point(2,2),Point(3,3)), List())),
    ("C 1 1 2 2 3 ,3 1 1 2 2 3 ,3", NonEmptyList[C](C(Point(1,1),Point(2,2),Point(3,3)), List(C(Point(1,1),Point(2,2),Point(3,3))))),
    ("C    1 1 2 2 3 ,3 1 1  2.55 2 3 ,3", NonEmptyList[C](C(Point(1,1),Point(2,2),Point(3,3)), List(C(Point(1,1),Point(2.55,2),Point(3,3))))),
    ("C    1 1 2 2 3 ,3 1 1  2.55 2 3 ,3 5 5,   9 9, 1392 0", NonEmptyList[C](C(Point(1,1),Point(2,2),Point(3,3)), List(C(Point(1,1),Point(2.55,2),Point(3,3)),
      C(Point(5,5),Point(9,9),Point(1392,0))))),
  ).foreach(testCommand(_, _, PathParse.curveToC))

  List(
    ("c 1 1 2 2 3 3", NonEmptyList[c_](c_(Point(1,1),Point(2,2),Point(3,3)), List())),
    ("c 1  1  2  2 3 3", NonEmptyList[c_](c_(Point(1,1),Point(2,2),Point(3,3)), List())),
    ("c 1, 1 2, 2 3, 3", NonEmptyList[c_](c_(Point(1,1),Point(2,2),Point(3,3)), List())),
    ("c 1 1 .2 2.55 3 3", NonEmptyList[c_](c_(Point(1,1),Point(.2,2.55),Point(3,3)), List())),
    ("c1 1 2 2 3 ,3 ", NonEmptyList[c_](c_(Point(1,1),Point(2,2),Point(3,3)), List())),
    ("c 1 1 2 2 3 ,3 1 1 2 2 3 ,3", NonEmptyList[c_](c_(Point(1,1),Point(2,2),Point(3,3)), List(c_(Point(1,1),Point(2,2),Point(3,3))))),
    ("c    1 1 2 2 3 ,3 1 1  2.55 2 3 ,3", NonEmptyList[c_](c_(Point(1,1),Point(2,2),Point(3,3)), List(c_(Point(1,1),Point(2.55,2),Point(3,3))))),
    ("c     1 1 2 2 3 ,3 1 1  2.55 2 3 ,3 5 5,   9 9, 1392 0", NonEmptyList[c_](c_(Point(1,1),Point(2,2),Point(3,3)), List(c_(Point(1,1),Point(2.55,2),Point(3,3)),
      c_(Point(5,5),Point(9,9),Point(1392,0))))),
  ).foreach(testCommand(_, _, PathParse.curveToC_))

  // smooth curveto
  List(
    ("S 1 1 2 2", NonEmptyList[S](S(Point(1,1),Point(2,2)), List())),
    ("S 1  1  2  2", NonEmptyList[S](S(Point(1,1),Point(2,2)), List())),
    ("S 1, 1 2, 2", NonEmptyList[S](S(Point(1,1),Point(2,2)), List())),
    ("S 1 1 .2 2.55", NonEmptyList[S](S(Point(1,1),Point(.2,2.55)), List())),
    ("S1 1 2 2 , ", NonEmptyList[S](S(Point(1,1),Point(2,2)), List())),
    ("S 1 1 2 2  1 1 2 2 ", NonEmptyList[S](S(Point(1,1),Point(2,2)), List(S(Point(1,1),Point(2,2))))),
    ("S    1 1 2 2  1 1  2.55 2 ", NonEmptyList[S](S(Point(1,1),Point(2,2)), List(S(Point(1,1),Point(2.55,2))))),
    ("S     1 1 2 2  1 1  2.55 2  5 5,  1392 0", NonEmptyList[S](S(Point(1,1),Point(2,2)), List(S(Point(1,1),Point(2.55,2)),
      S(Point(5,5),Point(1392,0))))),
  ).foreach(testCommand(_, _, PathParse.smoothCurvetoS))

  List(
    ("s 1 1 2 2", NonEmptyList[s_](s_(Point(1,1),Point(2,2)), List())),
    ("s1  1  2  2", NonEmptyList[s_](s_(Point(1,1),Point(2,2)), List())),
    ("s 1, 1 2, 2", NonEmptyList[s_](s_(Point(1,1),Point(2,2)), List())),
    ("s 1 1 .2 2.55", NonEmptyList[s_](s_(Point(1,1),Point(.2,2.55)), List())),
    ("s1 1 2 2 , ", NonEmptyList[s_](s_(Point(1,1),Point(2,2)), List())),
    ("s 1 1 2 2  1 1 2 2 ", NonEmptyList[s_](s_(Point(1,1),Point(2,2)), List(s_(Point(1,1),Point(2,2))))),
    ("s    1 1 2 2  1 1  2.55 2 ", NonEmptyList[s_](s_(Point(1,1),Point(2,2)), List(s_(Point(1,1),Point(2.55,2))))),
    ("s     1 1 2 2  1 1  2.55 2  5 5,  1392 0", NonEmptyList[s_](s_(Point(1,1),Point(2,2)), List(s_(Point(1,1),Point(2.55,2)),
      s_(Point(5,5),Point(1392,0))))),
  ).foreach(testCommand(_, _, PathParse.smoothCurvetoS_))

  // bezier curveto
  List(
    ("Q 1 1 2 2", NonEmptyList[Q](Q(Point(1,1),Point(2,2)), List())),
    ("Q 1  1  2  2", NonEmptyList[Q](Q(Point(1,1),Point(2,2)), List())),
    ("Q 1, 1 2, 2", NonEmptyList[Q](Q(Point(1,1),Point(2,2)), List())),
    ("Q 1 1 .2 2.55", NonEmptyList[Q](Q(Point(1,1),Point(.2,2.55)), List())),
    ("Q 1 1 2 2 , ", NonEmptyList[Q](Q(Point(1,1),Point(2,2)), List())),
    ("Q1 1 2 2  1 1 2 2 ", NonEmptyList[Q](Q(Point(1,1),Point(2,2)), List(Q(Point(1,1),Point(2,2))))),
    ("Q    1 1 2 2  1 1  2.55 2 ", NonEmptyList[Q](Q(Point(1,1),Point(2,2)), List(Q(Point(1,1),Point(2.55,2))))),
    ("Q     1 1 2 2  1 1  2.55 2  5 5,  1392 0", NonEmptyList[Q](Q(Point(1,1),Point(2,2)), List(Q(Point(1,1),Point(2.55,2)),
      Q(Point(5,5),Point(1392,0))))),
  ).foreach(testCommand(_, _, PathParse.quadraticBezierCurvetoQ))

  List(
    ("q 1 1 2 2", NonEmptyList[q_](q_(Point(1,1),Point(2,2)), List())),
    ("q 1  1  2  2", NonEmptyList[q_](q_(Point(1,1),Point(2,2)), List())),
    ("q 1, 1 2, 2", NonEmptyList[q_](q_(Point(1,1),Point(2,2)), List())),
    ("q 1 1 .2 2.55", NonEmptyList[q_](q_(Point(1,1),Point(.2,2.55)), List())),
    ("q 1 1 2 2 , ", NonEmptyList[q_](q_(Point(1,1),Point(2,2)), List())),
    ("q1 1 2 2  1 1 2 2 ", NonEmptyList[q_](q_(Point(1,1),Point(2,2)), List(q_(Point(1,1),Point(2,2))))),
    ("q    1 1 2 2  1 1  2.55 2 ", NonEmptyList[q_](q_(Point(1,1),Point(2,2)), List(q_(Point(1,1),Point(2.55,2))))),
    ("q     1 1 2 2  1 1  2.55 2  5 5,  1392 0", NonEmptyList[q_](q_(Point(1,1),Point(2,2)), List(q_(Point(1,1),Point(2.55,2)),
      q_(Point(5,5),Point(1392,0))))),
  ).foreach(testCommand(_, _, PathParse.quadraticBezierCurvetoQ_))

  // smooth curveto
  List(
    ("T 1 2", NonEmptyList[T](T(Point(1, 2)), List())),
    ("T 1 2", NonEmptyList[T](T(Point(1, 2)), List())),
    ("T1, 2", NonEmptyList[T](T(Point(1, 2)), List())),
    ("T 1,   2", NonEmptyList[T](T(Point(1, 2)), List())),
    ("T 1, 2 ", NonEmptyList[T](T(Point(1, 2)), List())),
    ("T 1, 2 15 255", NonEmptyList[T](T(Point(1, 2)), List(T(Point(15, 255))))),
    ("T 1.4, 2 15 255", NonEmptyList[T](T(Point(1.4, 2)), List(T(Point(15, 255))))),
  ).foreach(testCommand(_, _, PathParse.smoothQuadraticBezierCurvetoT))

  List(
    ("t 1 2", NonEmptyList[t_](t_(Point(1, 2)), List())),
    ("t 1 2", NonEmptyList[t_](t_(Point(1, 2)), List())),
    ("t1, 2", NonEmptyList[t_](t_(Point(1, 2)), List())),
    ("t 1,   2", NonEmptyList[t_](t_(Point(1, 2)), List())),
    ("t 1, 2 ", NonEmptyList[t_](t_(Point(1, 2)), List())),
    ("t 1, 2 15 255", NonEmptyList[t_](t_(Point(1, 2)), List(t_(Point(15, 255))))),
    ("t 1.4, 2 15 255", NonEmptyList[t_](t_(Point(1.4, 2)), List(t_(Point(15, 255))))),
  ).foreach(testCommand(_, _, PathParse.smoothQuadraticBezierCurvetoT_))

  // arc
  List(
    ("A 1 2 3 1 1 5 6", NonEmptyList[A](A(1, 2, 3, 1.toShort, 1.toShort, Point(5, 6)), List())),
    ("A1,2 ,3,1,1 5,6 ", NonEmptyList[A](A(1, 2, 3, 1, 1, Point(5, 6)), List())),
    ("A 1  2 3    1 1   " +
      "5 6", NonEmptyList[A](A(1, 2, 3, 1, 1, Point(5, 6)), List())),
    ("A 1 2 3 1 1 5 6 1 2 3 1 1 5 6 ", NonEmptyList[A](A(1, 2, 3, 1, 1, Point(5, 6)), List(A(1, 2, 3, 1, 1, Point(5, 6))))),
  ).foreach(testCommand(_, _, PathParse.ellipticalArcA))

  List(
    ("a 1 2 3 1 1 5 6", NonEmptyList[a_](a_(1, 2, 3, 1.toShort, 1.toShort, Point(5, 6)), List())),
    ("a1,2 ,3,1,1 5,6 ", NonEmptyList[a_](a_(1, 2, 3, 1, 1, Point(5, 6)), List())),
    ("a 1  2 3    1 1   " +
      "5 6", NonEmptyList[a_](a_(1, 2, 3, 1, 1, Point(5, 6)), List())),
    ("a 1 2 3 1 1 5 6 1 2 3 1 1 5 6 ", NonEmptyList[a_](a_(1, 2, 3, 1, 1, Point(5, 6)), List(a_(1, 2, 3, 1, 1, Point(5, 6))))),
  ).foreach(testCommand(_, _, PathParse.ellipticalArcA_))

  List(
    ("z", NonEmptyList[Z](Z(), List())),
    ("Z", NonEmptyList[Z](Z(), List())),
    ("z ", NonEmptyList[Z](Z(), List())),
    ("Z ", NonEmptyList[Z](Z(), List())),
  ).foreach(testCommand(_, _, PathParse.closePath))

  List(
    ("Z ", NonEmptyList[Z](Z(), List())),
    ("a 1 2 3 1 1 5 6 1 2 3 1 1 5 6 ", NonEmptyList[a_](a_(1, 2, 3, 1, 1, Point(5, 6)), List(a_(1, 2, 3, 1, 1, Point(5, 6))))),
    ("A 1 2 3 1 1 5 6 1 2 3 1 1 5 6 ", NonEmptyList[A](A(1, 2, 3, 1, 1, Point(5, 6)), List(A(1, 2, 3, 1, 1, Point(5, 6))))),
    ("t 1.4, 2 15 255", NonEmptyList[t_](t_(Point(1.4, 2)), List(t_(Point(15, 255))))),
    ("T 1.4, 2 15 255", NonEmptyList[T](T(Point(1.4, 2)), List(T(Point(15, 255))))),
    ("q     1 1 2 2  1 1  2.55 2  5 5,  1392 0", NonEmptyList[q_](q_(Point(1,1),Point(2,2)), List(q_(Point(1,1),Point(2.55,2)),
      q_(Point(5,5),Point(1392,0))))),
    ("Q     1 1 2 2  1 1  2.55 2  5 5,  1392 0", NonEmptyList[Q](Q(Point(1,1),Point(2,2)), List(Q(Point(1,1),Point(2.55,2)),
      Q(Point(5,5),Point(1392,0))))),
    ("s     1 1 2 2  1 1  2.55 2  5 5,  1392 0", NonEmptyList[s_](s_(Point(1,1),Point(2,2)), List(s_(Point(1,1),Point(2.55,2)),
      s_(Point(5,5),Point(1392,0))))),
    ("S     1 1 2 2  1 1  2.55 2  5 5,  1392 0", NonEmptyList[S](S(Point(1,1),Point(2,2)), List(S(Point(1,1),Point(2.55,2)),
      S(Point(5,5),Point(1392,0))))),
    ("c     1 1 2 2 3 ,3 1 1  2.55 2 3 ,3 5 5,   9 9, 1392 0", NonEmptyList[c_](c_(Point(1,1),Point(2,2),Point(3,3)), List(c_(Point(1,1),Point(2.55,2),Point(3,3)),
      c_(Point(5,5),Point(9,9),Point(1392,0))))),
    ("C    1 1 2 2 3 ,3 1 1  2.55 2 3 ,3 5 5,   9 9, 1392 0", NonEmptyList[C](C(Point(1,1),Point(2,2),Point(3,3)), List(C(Point(1,1),Point(2.55,2),Point(3,3)),
      C(Point(5,5),Point(9,9),Point(1392,0))))),
    ("v 1.4, 2 15 255", NonEmptyList[v_](v_(1.4), List(v_(2), v_(15), v_(255)))),
    ("V 1.4, 2 15 255", NonEmptyList[V](V(1.4), List(V(2), V(15), V(255)))),
    ("h 1.4, 2 15 255E-1", NonEmptyList[h_](h_(1.4), List(h_(2), h_(15), h_(25.5)))),
    ("H 1.4, 2 15 255", NonEmptyList[H](H(1.4), List(H(2), H(15), H(255)))),
    ("l 1.4, 2 15 255", NonEmptyList[l_](l_(Point(1.4, 2)), List(l_(Point(15, 255))))),
    ("L 1.4, 2 15 255E1", NonEmptyList[L](L(Point(1.4, 2)), List(L(Point(15, 2550))))),
    ("m 1.4, 2 15 255", NonEmptyList[m_](m_(Point(1.4, 2)), List(m_(Point(15, 255))))),
    ("M 1.4, 2 15 255", NonEmptyList[M](M(Point(1.4, 2)), List(M(Point(15, 255))))),
  ).foreach(testCommand(_, _, PathParse.svgCommand))

  List(
    ("M 1, 2 V 1", NonEmptyList(M(Point(1,2)), List(V(1)))),
    ("M 1, 2 V 1 V 2E2", NonEmptyList(M(Point(1,2)), List(V(1),V(200) ))),
    ("M 1, 2 1, 2 V 1 V 2E2", NonEmptyList(M(Point(1,2)), List(M(Point(1,2)),V(1),V(200) ))),
    ("M 1, 2 1, 2 V 1 V 2E2 m 22 33", NonEmptyList(M(Point(1,2)), List(M(Point(1,2)),V(1),V(200), m_(Point(22,33))))),
    ("M231 637Q204 637 199 638", NonEmptyList(M(Point(231,637)), List(Q(Point(204,637), Point(199, 638)))))
  ).foreach(testCommand0(_, _, PathParse.svgCommandRep))

  List(
    ("M 1, 2 V 1", NonEmptyList(M(Point(1, 2)), List(V(1)))),
    ("M 1, 2 V 1 V 2E2", NonEmptyList(M(Point(1, 2)), List(V(1), V(200)))),
    ("M 1, 2 1, 2 V 1 V 2E2", NonEmptyList(M(Point(1, 2)), List(M(Point(1, 2)), V(1), V(200)))),
    ("M 1, 2 1, 2 V 1 V 2E2 m 22 33", NonEmptyList(M(Point(1, 2)), List(M(Point(1, 2)), V(1), V(200), m_(Point(22, 33))))),
    ("M231 637Q204 637 199 638", NonEmptyList(M(Point(231, 637)), List(Q(Point(204, 637), Point(199, 638))))),
    ("M370 305T349 305T313 320T297 358Q297 381 312 396Q317" +
      " 401 317 402T307 404Q281 408 258 408Q209 408 178 " +
      "376Q131 329 131 219Q131 137 162 90Q203 29 272 29Q313 " +
      "29 338 55T374 117Q376 125 379 127T395 129H409Q415 123 " +
      "415 120Q415 116 411 104T395 71T366 33T318 2T249 -11Q163" +
      " -11 99 53T34 214Q34 318 99 383T250 448T370 421T404" +
      " 357Q404 334 387 320Z", NonEmptyList.of(
      M(Point(370, 305)),
      T(Point(349, 305)),
      T(Point(313.0,320.0)),
      T(Point(297.0,358.0)),
      Q(Point(297.0,381.0), Point(312.0,396.0)),
      Q(Point(317.0,401.0), Point(317.0,402.0)),
      T(Point(307.0,404.0)),
      Q(Point(281, 408), Point(258, 408)),
      Q(Point(209, 408), Point(178, 376)),
      Q(Point(131, 329), Point(131, 219)),
      Q(Point(131, 137), Point(162, 90)),
      Q(Point(203, 29), Point(272, 29)),
      Q(Point(313, 29), Point(338, 55)),
      T(Point(374, 117)),
      Q(Point(376, 125), Point(379, 127)),
      T(Point(395, 129)),
      H(409),
      Q(Point(415, 123), Point(415, 120)),
      Q(Point(415, 116), Point(411, 104)),
      T(Point(395, 71)),
      T(Point(366, 33)),
      T(Point(318, 2)),
      T(Point(249, -11)),
      Q(Point(163, -11), Point(99, 53)),
      T(Point(34, 214)),
      Q(Point(34, 318),  Point(99, 383)),
      T(Point(250, 448)),
      T(Point(370, 421)),
      T(Point(404, 357)),
      Q(Point(404, 334), Point(387, 320)),
      Z())
    )).foreach(testCommand0(_, _, PathParse.svgPath))
}