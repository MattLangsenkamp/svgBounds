import cats.data.NonEmptyList
import com.dprl.*
import cats.parse.{Parser, Parser0}

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
    ("M 1 2", NonEmptyList[M](M(1, 2), List())),
    ("M 1 2", NonEmptyList[M](M(1, 2), List())),
    ("M 1, 2", NonEmptyList[M](M(1, 2), List())),
    ("M 1,   2", NonEmptyList[M](M(1, 2), List())),
    ("M 1, 2 ", NonEmptyList[M](M(1, 2), List())),
    ("M1, 2 15 255", NonEmptyList[M](M(1, 2), List(M(15, 255)))),
    ("M 1.4, 2 15 255", NonEmptyList[M](M(1.4, 2), List(M(15, 255)))),
  ).foreach(testCommand(_, _, PathParse.movetoM))

  List(
    ("m 1 2", NonEmptyList[m_](m_(1, 2), List())),
    ("m 1 2", NonEmptyList[m_](m_(1, 2), List())),
    ("m 1, 2", NonEmptyList[m_](m_(1, 2), List())),
    ("m 1,   2", NonEmptyList[m_](m_(1, 2), List())),
    ("m1, 2 ", NonEmptyList[m_](m_(1, 2), List())),
    ("m 1, 2 15 255", NonEmptyList[m_](m_(1, 2), List(m_(15, 255)))),
    ("m 1.4, 2 15 255", NonEmptyList[m_](m_(1.4, 2), List(m_(15, 255)))),
  ).foreach(testCommand(_, _, PathParse.movetoM_))

  // ClosePath tests

  // LineToTests
  List(
    ("L 1 2", NonEmptyList[L](L(1, 2), List())),
    ("L1 2", NonEmptyList[L](L(1, 2), List())),
    ("L1, 2", NonEmptyList[L](L(1, 2), List())),
    ("L 1,   2", NonEmptyList[L](L(1, 2), List())),
    ("L 1, 2 ", NonEmptyList[L](L(1, 2), List())),
    ("L1, 2 15 255", NonEmptyList[L](L(1, 2), List(L(15, 255)))),
    ("L 1.4, 2 15 255", NonEmptyList[L](L(1.4, 2), List(L(15, 255)))),
  ).foreach(testCommand(_, _, PathParse.linetoL))

  List(
    ("l 1 2", NonEmptyList[l_](l_(1, 2), List())),
    ("l 1 2", NonEmptyList[l_](l_(1, 2), List())),
    ("l1, 2", NonEmptyList[l_](l_(1, 2), List())),
    ("l 1,   2", NonEmptyList[l_](l_(1, 2), List())),
    ("l 1, 2 ", NonEmptyList[l_](l_(1, 2), List())),
    ("l 1, 2 15 255", NonEmptyList[l_](l_(1, 2), List(l_(15, 255)))),
    ("l 1.4, 2 15 255", NonEmptyList[l_](l_(1.4, 2), List(l_(15, 255)))),
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
    ("C 1 1 2 2 3 3", NonEmptyList[C](C(1,1,2,2,3,3), List())),
    ("C 1  1  2  2 3 3", NonEmptyList[C](C(1,1,2,2,3,3), List())),
    ("C 1, 1 2, 2 3, 3", NonEmptyList[C](C(1,1,2,2,3,3), List())),
    ("C 1 1 .2 2.55 3 3", NonEmptyList[C](C(1,1,.2,2.55,3,3), List())),
    ("C1 1 2 2 3 ,3 ", NonEmptyList[C](C(1,1,2,2,3,3), List())),
    ("C 1 1 2 2 3 ,3 1 1 2 2 3 ,3", NonEmptyList[C](C(1,1,2,2,3,3), List(C(1,1,2,2,3,3)))),
    ("C    1 1 2 2 3 ,3 1 1  2.55 2 3 ,3", NonEmptyList[C](C(1,1,2,2,3,3), List(C(1,1,2.55,2,3,3)))),
    ("C    1 1 2 2 3 ,3 1 1  2.55 2 3 ,3 5 5,   9 9, 1392 0", NonEmptyList[C](C(1,1,2,2,3,3), List(C(1,1,2.55,2,3,3),
      C(5,5,9,9,1392,0)))),
  ).foreach(testCommand(_, _, PathParse.curveToC))

  List(
    ("c 1 1 2 2 3 3", NonEmptyList[c_](c_(1,1,2,2,3,3), List())),
    ("c 1  1  2  2 3 3", NonEmptyList[c_](c_(1,1,2,2,3,3), List())),
    ("c 1, 1 2, 2 3, 3", NonEmptyList[c_](c_(1,1,2,2,3,3), List())),
    ("c 1 1 .2 2.55 3 3", NonEmptyList[c_](c_(1,1,.2,2.55,3,3), List())),
    ("c1 1 2 2 3 ,3 ", NonEmptyList[c_](c_(1,1,2,2,3,3), List())),
    ("c 1 1 2 2 3 ,3 1 1 2 2 3 ,3", NonEmptyList[c_](c_(1,1,2,2,3,3), List(c_(1,1,2,2,3,3)))),
    ("c    1 1 2 2 3 ,3 1 1  2.55 2 3 ,3", NonEmptyList[c_](c_(1,1,2,2,3,3), List(c_(1,1,2.55,2,3,3)))),
    ("c     1 1 2 2 3 ,3 1 1  2.55 2 3 ,3 5 5,   9 9, 1392 0", NonEmptyList[c_](c_(1,1,2,2,3,3), List(c_(1,1,2.55,2,3,3),
      c_(5,5,9,9,1392,0)))),
  ).foreach(testCommand(_, _, PathParse.curveToC_))

  // smooth curveto
  List(
    ("S 1 1 2 2", NonEmptyList[S](S(1,1,2,2), List())),
    ("S 1  1  2  2", NonEmptyList[S](S(1,1,2,2), List())),
    ("S 1, 1 2, 2", NonEmptyList[S](S(1,1,2,2), List())),
    ("S 1 1 .2 2.55", NonEmptyList[S](S(1,1,.2,2.55), List())),
    ("S1 1 2 2 , ", NonEmptyList[S](S(1,1,2,2), List())),
    ("S 1 1 2 2  1 1 2 2 ", NonEmptyList[S](S(1,1,2,2), List(S(1,1,2,2)))),
    ("S    1 1 2 2  1 1  2.55 2 ", NonEmptyList[S](S(1,1,2,2), List(S(1,1,2.55,2)))),
    ("S     1 1 2 2  1 1  2.55 2  5 5,  1392 0", NonEmptyList[S](S(1,1,2,2), List(S(1,1,2.55,2),
      S(5,5,1392,0)))),
  ).foreach(testCommand(_, _, PathParse.smoothCurvetoS))

  List(
    ("s 1 1 2 2", NonEmptyList[s_](s_(1,1,2,2), List())),
    ("s1  1  2  2", NonEmptyList[s_](s_(1,1,2,2), List())),
    ("s 1, 1 2, 2", NonEmptyList[s_](s_(1,1,2,2), List())),
    ("s 1 1 .2 2.55", NonEmptyList[s_](s_(1,1,.2,2.55), List())),
    ("s1 1 2 2 , ", NonEmptyList[s_](s_(1,1,2,2), List())),
    ("s 1 1 2 2  1 1 2 2 ", NonEmptyList[s_](s_(1,1,2,2), List(s_(1,1,2,2)))),
    ("s    1 1 2 2  1 1  2.55 2 ", NonEmptyList[s_](s_(1,1,2,2), List(s_(1,1,2.55,2)))),
    ("s     1 1 2 2  1 1  2.55 2  5 5,  1392 0", NonEmptyList[s_](s_(1,1,2,2), List(s_(1,1,2.55,2),
      s_(5,5,1392,0)))),
  ).foreach(testCommand(_, _, PathParse.smoothCurvetoS_))

  // bezier curveto
  List(
    ("Q 1 1 2 2", NonEmptyList[Q](Q(1,1,2,2), List())),
    ("Q 1  1  2  2", NonEmptyList[Q](Q(1,1,2,2), List())),
    ("Q 1, 1 2, 2", NonEmptyList[Q](Q(1,1,2,2), List())),
    ("Q 1 1 .2 2.55", NonEmptyList[Q](Q(1,1,.2,2.55), List())),
    ("Q 1 1 2 2 , ", NonEmptyList[Q](Q(1,1,2,2), List())),
    ("Q1 1 2 2  1 1 2 2 ", NonEmptyList[Q](Q(1,1,2,2), List(Q(1,1,2,2)))),
    ("Q    1 1 2 2  1 1  2.55 2 ", NonEmptyList[Q](Q(1,1,2,2), List(Q(1,1,2.55,2)))),
    ("Q     1 1 2 2  1 1  2.55 2  5 5,  1392 0", NonEmptyList[Q](Q(1,1,2,2), List(Q(1,1,2.55,2),
      Q(5,5,1392,0)))),
  ).foreach(testCommand(_, _, PathParse.quadraticBezierCurvetoQ))

  List(
    ("q 1 1 2 2", NonEmptyList[q_](q_(1,1,2,2), List())),
    ("q 1  1  2  2", NonEmptyList[q_](q_(1,1,2,2), List())),
    ("q 1, 1 2, 2", NonEmptyList[q_](q_(1,1,2,2), List())),
    ("q 1 1 .2 2.55", NonEmptyList[q_](q_(1,1,.2,2.55), List())),
    ("q 1 1 2 2 , ", NonEmptyList[q_](q_(1,1,2,2), List())),
    ("q1 1 2 2  1 1 2 2 ", NonEmptyList[q_](q_(1,1,2,2), List(q_(1,1,2,2)))),
    ("q    1 1 2 2  1 1  2.55 2 ", NonEmptyList[q_](q_(1,1,2,2), List(q_(1,1,2.55,2)))),
    ("q     1 1 2 2  1 1  2.55 2  5 5,  1392 0", NonEmptyList[q_](q_(1,1,2,2), List(q_(1,1,2.55,2),
      q_(5,5,1392,0)))),
  ).foreach(testCommand(_, _, PathParse.quadraticBezierCurvetoQ_))

  // smooth curveto
  List(
    ("T 1 2", NonEmptyList[T](T(1, 2), List())),
    ("T 1 2", NonEmptyList[T](T(1, 2), List())),
    ("T1, 2", NonEmptyList[T](T(1, 2), List())),
    ("T 1,   2", NonEmptyList[T](T(1, 2), List())),
    ("T 1, 2 ", NonEmptyList[T](T(1, 2), List())),
    ("T 1, 2 15 255", NonEmptyList[T](T(1, 2), List(T(15, 255)))),
    ("T 1.4, 2 15 255", NonEmptyList[T](T(1.4, 2), List(T(15, 255)))),
  ).foreach(testCommand(_, _, PathParse.smoothQuadraticBezierCurvetoT))

  List(
    ("t 1 2", NonEmptyList[t_](t_(1, 2), List())),
    ("t 1 2", NonEmptyList[t_](t_(1, 2), List())),
    ("t1, 2", NonEmptyList[t_](t_(1, 2), List())),
    ("t 1,   2", NonEmptyList[t_](t_(1, 2), List())),
    ("t 1, 2 ", NonEmptyList[t_](t_(1, 2), List())),
    ("t 1, 2 15 255", NonEmptyList[t_](t_(1, 2), List(t_(15, 255)))),
    ("t 1.4, 2 15 255", NonEmptyList[t_](t_(1.4, 2), List(t_(15, 255)))),
  ).foreach(testCommand(_, _, PathParse.smoothQuadraticBezierCurvetoT_))

  // arc
  List(
    ("A 1 2 3 1 1 5 6", NonEmptyList[A](A(1, 2, 3, 1.toShort, 1.toShort, 5, 6), List())),
    ("A1,2 ,3,1,1 5,6 ", NonEmptyList[A](A(1, 2, 3, 1, 1, 5, 6), List())),
    ("A 1  2 3    1 1   " +
      "5 6", NonEmptyList[A](A(1, 2, 3, 1, 1, 5, 6), List())),
    ("A 1 2 3 1 1 5 6 1 2 3 1 1 5 6 ", NonEmptyList[A](A(1, 2, 3, 1, 1, 5, 6), List(A(1, 2, 3, 1, 1, 5, 6)))),
  ).foreach(testCommand(_, _, PathParse.ellipticalArcA))

  List(
    ("a 1 2 3 1 1 5 6", NonEmptyList[a_](a_(1, 2, 3, 1.toShort, 1.toShort, 5, 6), List())),
    ("a1,2 ,3,1,1 5,6 ", NonEmptyList[a_](a_(1, 2, 3, 1, 1, 5, 6), List())),
    ("a 1  2 3    1 1   " +
      "5 6", NonEmptyList[a_](a_(1, 2, 3, 1, 1, 5, 6), List())),
    ("a 1 2 3 1 1 5 6 1 2 3 1 1 5 6 ", NonEmptyList[a_](a_(1, 2, 3, 1, 1, 5, 6), List(a_(1, 2, 3, 1, 1, 5, 6)))),
  ).foreach(testCommand(_, _, PathParse.ellipticalArcA_))

  List(
    ("z", NonEmptyList[Z](Z(), List())),
    ("Z", NonEmptyList[Z](Z(), List())),
    ("z ", NonEmptyList[Z](Z(), List())),
    ("Z ", NonEmptyList[Z](Z(), List())),
  ).foreach(testCommand(_, _, PathParse.closePath))

  List(
    ("Z ", NonEmptyList[Z](Z(), List())),
    ("a 1 2 3 1 1 5 6 1 2 3 1 1 5 6 ", NonEmptyList[a_](a_(1, 2, 3, 1, 1, 5, 6), List(a_(1, 2, 3, 1, 1, 5, 6)))),
    ("A 1 2 3 1 1 5 6 1 2 3 1 1 5 6 ", NonEmptyList[A](A(1, 2, 3, 1, 1, 5, 6), List(A(1, 2, 3, 1, 1, 5, 6)))),
    ("t 1.4, 2 15 255", NonEmptyList[t_](t_(1.4, 2), List(t_(15, 255)))),
    ("T 1.4, 2 15 255", NonEmptyList[T](T(1.4, 2), List(T(15, 255)))),
    ("q     1 1 2 2  1 1  2.55 2  5 5,  1392 0", NonEmptyList[q_](q_(1,1,2,2), List(q_(1,1,2.55,2),
      q_(5,5,1392,0)))),
    ("Q     1 1 2 2  1 1  2.55 2  5 5,  1392 0", NonEmptyList[Q](Q(1,1,2,2), List(Q(1,1,2.55,2),
      Q(5,5,1392,0)))),
    ("s     1 1 2 2  1 1  2.55 2  5 5,  1392 0", NonEmptyList[s_](s_(1,1,2,2), List(s_(1,1,2.55,2),
      s_(5,5,1392,0)))),
    ("S     1 1 2 2  1 1  2.55 2  5 5,  1392 0", NonEmptyList[S](S(1,1,2,2), List(S(1,1,2.55,2),
      S(5,5,1392,0)))),
    ("c     1 1 2 2 3 ,3 1 1  2.55 2 3 ,3 5 5,   9 9, 1392 0", NonEmptyList[c_](c_(1,1,2,2,3,3), List(c_(1,1,2.55,2,3,3),
      c_(5,5,9,9,1392,0)))),
    ("C    1 1 2 2 3 ,3 1 1  2.55 2 3 ,3 5 5,   9 9, 1392 0", NonEmptyList[C](C(1,1,2,2,3,3), List(C(1,1,2.55,2,3,3),
      C(5,5,9,9,1392,0)))),
    ("v 1.4, 2 15 255", NonEmptyList[v_](v_(1.4), List(v_(2), v_(15), v_(255)))),
    ("V 1.4, 2 15 255", NonEmptyList[V](V(1.4), List(V(2), V(15), V(255)))),
    ("h 1.4, 2 15 255E-1", NonEmptyList[h_](h_(1.4), List(h_(2), h_(15), h_(25.5)))),
    ("H 1.4, 2 15 255", NonEmptyList[H](H(1.4), List(H(2), H(15), H(255)))),
    ("l 1.4, 2 15 255", NonEmptyList[l_](l_(1.4, 2), List(l_(15, 255)))),
    ("L 1.4, 2 15 255E1", NonEmptyList[L](L(1.4, 2), List(L(15, 2550)))),
    ("m 1.4, 2 15 255", NonEmptyList[m_](m_(1.4, 2), List(m_(15, 255)))),
    ("M 1.4, 2 15 255", NonEmptyList[M](M(1.4, 2), List(M(15, 255)))),
  ).foreach(testCommand(_, _, PathParse.svgCommand))

  List(
    ("M 1, 2 V 1", NonEmptyList(M(1,2), List(V(1)))),
    ("M 1, 2 V 1 V 2E2", NonEmptyList(M(1,2), List(V(1),V(200) ))),
    ("M 1, 2 1, 2 V 1 V 2E2", NonEmptyList(M(1,2), List(M(1,2),V(1),V(200) ))),
    ("M 1, 2 1, 2 V 1 V 2E2 m 22 33", NonEmptyList(M(1,2), List(M(1,2),V(1),V(200), m_(22,33) ))),
  ).foreach(testCommand0(_, _, PathParse.svgCommandRep))
}