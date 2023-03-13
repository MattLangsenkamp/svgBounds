package org.dprl.svgbounds

import cats.data.NonEmptyList
import cats.parse.Parser.{char, charIn, string}
import cats.parse.{Parser, Parser0}
import org.dprl.svgbounds.model.SvgType.Point
import org.dprl.svgbounds.model.SvgCommand.*


object PathParse {


  // the following was derived from the grammar provided in https://svgwg.org/svg2-draft/paths.html#PathElement

  val decimal: Parser[Unit] = char('.')
  val flag: Parser[String] = string(charIn("10"))
  val digit: Parser[Char] = charIn("0123456789")
  val wsp: Parser[Char] = charIn("\t \n\f\r")
  val comma: Parser[Unit] = char(44)
  val commaWsp: Parser[String] = (wsp.rep ~ comma.? ~ wsp.rep0).string | (comma ~ wsp.rep0).string
  val fractionConstant: Parser[String] = (digit.rep ~ decimal ~ digit.rep).string.backtrack | (decimal ~ digit.rep).string | string(digit.rep)
  val sign: Parser[Char] = charIn("+-")
  val exponent: Parser[String] = (charIn("eE") ~ sign.? ~ digit.rep0).string
  val number: Parser[String] = (fractionConstant ~ exponent.?).string
  val coordinate: Parser[Double] = ((sign ~ number).string | number).map(_.toDouble)
  val coordinatePair: Parser[Point] = ((coordinate <* commaWsp) ~ coordinate).map((x, y) => Point(x, y))
  val coordinateSequence: Parser[NonEmptyList[Double]] = (coordinate <* commaWsp.?).rep
  val coordinatePairSequence: Parser[NonEmptyList[Point]] = (coordinatePair <* commaWsp.?).rep
  val coordinatePairDouble: Parser[(Point, Point)] = (coordinatePair <* commaWsp) ~ coordinatePair
  val coordinatePairTriple: Parser[(Point, Point, Point)] =
    (((coordinatePair <* commaWsp) ~ coordinatePair <* commaWsp) ~ coordinatePair).map {
      case ((coord1, coord2), coord3) => (coord1, coord2, coord3)
    }

  // getting into actual stuff
  val ellipticalArcArgument: Parser[(Float, Float, Float, Short, Short, Point)] = ((number <* commaWsp)
    ~ (number <* commaWsp)
    ~ (number <* commaWsp)
    ~ (flag <* commaWsp)
    ~ (flag <* commaWsp)
    ~ coordinatePair)
    .map {
      case (((((s1, s2), s3), c1), c2), p) =>
        (s1.toFloat, s2.toFloat, s3.toFloat, c1.toShort, c2.toShort, p)
    }
  val ellipticalArcArgumentSequence: Parser[NonEmptyList[(Float, Float, Float, Short, Short, Point)]] =
    (ellipticalArcArgument <* commaWsp.?).rep

  val ellipticalArcA: Parser[NonEmptyList[A]] =
    ((char('A') ~ wsp.rep0) *> ellipticalArcArgumentSequence).map {
      case aList: NonEmptyList[(Float, Float, Float, Short, Short, Point)] =>
        aList.map((rx, ry, xAxisRotation, largeArcFlag, sweepFlag, finalPoint) =>
          A(rx, ry, xAxisRotation, largeArcFlag, sweepFlag, finalPoint))
    }

  val ellipticalArcA_ : Parser[NonEmptyList[a_]] =
    ((char('a') ~ wsp.rep0) *> ellipticalArcArgumentSequence).map {
      case aList: NonEmptyList[(Float, Float, Float, Short, Short, Point)] =>
        aList.map((rx, ry, xAxisRotation, largeArcFlag, sweepFlag, finalPoint) =>
          a_(rx, ry, xAxisRotation, largeArcFlag, sweepFlag, finalPoint))
    }

  // smooth quad
  val smoothQuadraticBezierCurvetoT: Parser[NonEmptyList[T]] =
    ((char('T') ~ wsp.rep0) *> coordinatePairSequence).map {
      case tList: NonEmptyList[Point] => tList.map(p => T(p))
    }

  val smoothQuadraticBezierCurvetoT_ : Parser[NonEmptyList[t_]] =
    ((char('t') ~ wsp.rep0) *> coordinatePairSequence).map {
      case tList: NonEmptyList[Point] => tList.map(p => t_(p))
    }

  // reg quad
  val quadraticBezierCurveCoordinateSequence: Parser[NonEmptyList[(Point, Point)]] =
    (coordinatePairDouble <* commaWsp.?).rep

  val quadraticBezierCurvetoQ: Parser[NonEmptyList[Q]] =
    ((char('Q') ~ wsp.rep0) *> quadraticBezierCurveCoordinateSequence).map {
      case qList: NonEmptyList[(Point, Point)] =>
        qList.map((controlPoint, finalPoint) => Q(controlPoint, finalPoint))
    };

  val quadraticBezierCurvetoQ_ : Parser[NonEmptyList[q_]] =
    ((char('q') ~ wsp.rep0) *> quadraticBezierCurveCoordinateSequence).map {
      case qList: NonEmptyList[(Point, Point)] =>
        qList.map((controlPoint, finalPoint) => q_(controlPoint, finalPoint))
    };

  // smooth curveto
  val smoothCurvetoCoordinateSequence: Parser[NonEmptyList[(Point, Point)]] =
    (coordinatePairDouble <* commaWsp.?).rep

  val smoothCurvetoS: Parser[NonEmptyList[S]] =
    ((char('S') ~ wsp.rep0) *> smoothCurvetoCoordinateSequence).map {
      case sList: NonEmptyList[(Point, Point)] =>
        sList.map((controlPoint, finalPoint) => S(controlPoint, finalPoint))
    };

  val smoothCurvetoS_ : Parser[NonEmptyList[s_]] =
    ((char('s') ~ wsp.rep0) *> smoothCurvetoCoordinateSequence).map {
      case sList: NonEmptyList[(Point, Point)] =>
        sList.map((controlPoint, finalPoint) => s_(controlPoint, finalPoint))
    };

  // reg curveto
  val curvetoCoordinateSequence: Parser[NonEmptyList[(Point, Point, Point)]] =
    (coordinatePairTriple <* commaWsp.?).rep

  val curveToC: Parser[NonEmptyList[C]] =
    ((char('C') ~ wsp.rep0) *> curvetoCoordinateSequence).map {
      case cList: NonEmptyList[(Point, Point, Point)] =>
        cList.map((controlPoint1, controlPoint2, finalPoint) =>
          C(controlPoint1, controlPoint2, finalPoint))
    }

  val curveToC_ : Parser[NonEmptyList[c_]] =
    ((char('c') ~ wsp.rep0) *> curvetoCoordinateSequence).map {
      case cList: NonEmptyList[(Point, Point, Point)] =>
        cList.map((controlPoint1, controlPoint2, finalPoint) =>
          c_(controlPoint1, controlPoint2, finalPoint))
    }

  // linetos
  val horizontalLinetoH: Parser[NonEmptyList[H]] = ((char('H') ~ wsp.rep0) *> coordinateSequence).map {
    case hList: NonEmptyList[Double] => hList.map(H.apply)
  }

  val horizontalLinetoH_ : Parser[NonEmptyList[h_]] = ((char('h') ~ wsp.rep0) *> coordinateSequence).map {
    case hList: NonEmptyList[Double] => hList.map(h_.apply)
  }

  val verticalLinetoV: Parser[NonEmptyList[V]] = ((char('V') ~ wsp.rep0) *> coordinateSequence).map {
    case vList: NonEmptyList[Double] => vList.map(V.apply)
  }

  val verticalLinetoV_ : Parser[NonEmptyList[v_]] = ((char('v') ~ wsp.rep0) *> coordinateSequence).map {
    case vList: NonEmptyList[Double] => vList.map(v_.apply)
  }

  val linetoL: Parser[NonEmptyList[L]] = ((char('L') ~ wsp.rep0) *> coordinatePairSequence).map {
    case lList: NonEmptyList[Point] => lList.map(L.apply)
  }

  val linetoL_ : Parser[NonEmptyList[l_]] = ((char('l') ~ wsp.rep0) *> coordinatePairSequence).map {
    case lList: NonEmptyList[Point] => lList.map(l_.apply)
  }

  // close path
  val closePath: Parser[NonEmptyList[Z]] = charIn("zZ").map(_ => NonEmptyList(Z(), List())) // no need for z_ really

  // moveto
  val movetoM: Parser[NonEmptyList[M]] = ((char('M') ~ wsp.rep0) *> coordinatePairSequence).map {
    case mList: NonEmptyList[Point] => mList.map(p => M(p))
  }

  val movetoM_ : Parser[NonEmptyList[m_]] = ((char('m') ~ wsp.rep0) *> coordinatePairSequence).map {
    case mList: NonEmptyList[Point] => mList.map(p => m_(p))
  }

  val svgCommand: Parser[NonEmptyList[SvgCommand]] = Parser.oneOf(
    List(
      movetoM,
      movetoM_,
      closePath,
      linetoL,
      linetoL_,
      horizontalLinetoH,
      horizontalLinetoH_,
      verticalLinetoV,
      verticalLinetoV_,
      curveToC,
      curveToC_,
      smoothCurvetoS,
      smoothCurvetoS_,
      quadraticBezierCurvetoQ,
      quadraticBezierCurvetoQ_,
      smoothQuadraticBezierCurvetoT,
      smoothQuadraticBezierCurvetoT_,
      ellipticalArcA,
      ellipticalArcA_
    )
  )

  val mCombined: Parser[NonEmptyList[SvgCommand]] = Parser.oneOf(List(movetoM, movetoM_))

  val svgCommandRep: Parser0[NonEmptyList[SvgCommand]] = svgCommand.rep0.map {
    case ::(head, next) => next.foldLeft(head)((svgCommandList, n) => svgCommandList ::: n)
    case Nil => NonEmptyList.of(m_(Point(0, 0)))
  }

  private val svgPathNoLeadingSpace: Parser[NonEmptyList[SvgCommand]] = (mCombined ~ svgCommandRep).map((l1, l2) => l1 ::: l2)

  private val svgPathLeadingSpace: Parser[NonEmptyList[SvgCommand]] = (wsp.rep *> mCombined ~ svgCommandRep).map((l1, l2) => l1 ::: l2)

  val svgPath: Parser[NonEmptyList[SvgCommand]] = svgPathLeadingSpace | svgPathNoLeadingSpace
}