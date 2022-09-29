package com.dprl
import cats.data.NonEmptyList
import cats.parse.{Parser, Parser0}
import cats.parse.Parser.{char, charIn, string}


object PathParse {


  // the following was derived from the grammar provided in https://svgwg.org/svg2-draft/paths.html#PathElement

  val decimal: Parser[Unit] = char('.')
  val flag: Parser[String] = string(charIn("10"))
  val digit: Parser[Char] = charIn("0123456789")
  val wsp: Parser[Char] = charIn("\t \n\f\r")
  val comma: Parser[Unit] = char(44)
  val commaWsp: Parser[String] = (wsp.rep ~ comma.? ~ wsp.rep0).string.backtrack | (comma ~ wsp.rep0).string
  val fractionConstant: Parser[String] = (digit.rep ~ decimal ~ digit.rep).string.backtrack | (decimal ~ digit.rep).string.backtrack | string(digit.rep)
  val sign: Parser[Char] = charIn("+-")
  val exponent: Parser[String] = (charIn("eE") ~ sign.? ~ digit.rep0).string
  val number: Parser[String] = (fractionConstant ~ exponent.?).string
  val coordinate: Parser[Float] = (sign ~ number).string.backtrack.map(_.toFloat) | number.map(_.toFloat)
  val coordinatePair: Parser[(Float, Float)] = (coordinate <* commaWsp) ~ coordinate
  val coordinateSequence: Parser[NonEmptyList[Float]] = (coordinate <* commaWsp.?).rep
  val coordinatePairSequence: Parser[NonEmptyList[(Float, Float)]] = (coordinatePair <* commaWsp.?).rep
  val coordinatePairDouble: Parser[((Float, Float), (Float, Float))] = (coordinatePair <* commaWsp) ~ coordinatePair
  val coordinatePairTriple: Parser[((Float, Float), (Float, Float), (Float, Float))] =
    (((coordinatePair <* commaWsp) ~ coordinatePair <* commaWsp) ~ coordinatePair).map{
      case ((coord1, coord2), coord3) => (coord1, coord2, coord3)
    }

  // getting into actual stuff
  val ellipticalArcArgument: Parser[(Float, Float, Float, Short, Short, (Float, Float))] = ((number <* commaWsp)
      ~ (number <* commaWsp)
      ~ (number <* commaWsp)
      ~ (flag <* commaWsp)
      ~ (flag <* commaWsp)
      ~ coordinatePair)
      .map {
        case (((((s1, s2), s3), c1), c2), (t1, t2)) =>
          (s1.toFloat, s2.toFloat, s3.toFloat, c1.toShort, c2.toShort, (t1, t2))}
  val ellipticalArcArgumentSequence: Parser[NonEmptyList[(Float, Float, Float, Short, Short, (Float, Float))]] =
    (ellipticalArcArgument <* commaWsp.?).rep

  val ellipticalArcA: Parser[NonEmptyList[A]] =
    ((char('A') ~ wsp.rep0) *> ellipticalArcArgumentSequence).map {
      case aList: NonEmptyList[(Float, Float, Float, Short, Short, (Float, Float))] =>
        aList.map((rx, ry, xAxisRotation, largeArcFlag, sweepFlag, finalPoint) =>
          A(rx, ry, xAxisRotation, largeArcFlag, sweepFlag, finalPoint._1, finalPoint._2))
    }

  val ellipticalArcA_ : Parser[NonEmptyList[a_]] =
    ((char('a') ~ wsp.rep0) *> ellipticalArcArgumentSequence).map {
      case aList: NonEmptyList[(Float, Float, Float, Short, Short, (Float, Float))] =>
        aList.map((rx, ry, xAxisRotation, largeArcFlag, sweepFlag, finalPoint) =>
          a_(rx, ry, xAxisRotation, largeArcFlag, sweepFlag, finalPoint._1, finalPoint._2))
    }

  // smooth quad
  val smoothQuadraticBezierCurvetoT: Parser[NonEmptyList[T]] =
    ((char('T') ~ wsp.rep0) *> coordinatePairSequence).map {
      case tList: NonEmptyList[(Float, Float)] => tList.map((x:Float, y: Float) => T(x, y))
    }

  val smoothQuadraticBezierCurvetoT_ : Parser[NonEmptyList[t_]] =
    ((char('t') ~ wsp.rep0) *> coordinatePairSequence).map {
      case tList: NonEmptyList[(Float, Float)] => tList.map((x:Float, y: Float) => t_(x, y))
    }

  // reg quad
  val quadraticBezierCurveCoordinateSequence: Parser[NonEmptyList[((Float, Float), (Float, Float))]] =
    (coordinatePairDouble <* commaWsp.?).rep

  val quadraticBezierCurvetoQ: Parser[NonEmptyList[Q]] =
    ((char('Q') ~ wsp.rep0) *> quadraticBezierCurveCoordinateSequence).map{
      case qList: NonEmptyList[((Float, Float), (Float, Float))] =>
        qList.map((controlPoint, finalPoint) => Q(controlPoint._1, controlPoint._2, finalPoint._1, finalPoint._2))
    };

  val quadraticBezierCurvetoQ_ : Parser[NonEmptyList[q_]] =
    ((char('q') ~ wsp.rep0) *> quadraticBezierCurveCoordinateSequence).map{
      case qList: NonEmptyList[((Float, Float), (Float, Float))] =>
        qList.map((controlPoint, finalPoint) => q_(controlPoint._1, controlPoint._2, finalPoint._1, finalPoint._2))
    };

  // smooth curveto
  val smoothCurvetoCoordinateSequence: Parser[NonEmptyList[((Float, Float), (Float, Float))]] =
    (coordinatePairDouble <* commaWsp.?).rep

  val smoothCurvetoS: Parser[NonEmptyList[S]] =
    ((char('S') ~ wsp.rep0) *> smoothCurvetoCoordinateSequence).map{
      case sList: NonEmptyList[((Float, Float), (Float, Float))] =>
        sList.map((controlPoint, finalPoint) => S(controlPoint._1, controlPoint._2, finalPoint._1, finalPoint._2))
    };

  val smoothCurvetoS_ : Parser[NonEmptyList[s_]] =
    ((char('s') ~ wsp.rep0) *> smoothCurvetoCoordinateSequence).map{
      case sList: NonEmptyList[((Float, Float), (Float, Float))] =>
        sList.map((controlPoint, finalPoint) => s_(controlPoint._1, controlPoint._2, finalPoint._1, finalPoint._2))
    };

  // reg curveto
  val curvetoCoordinateSequence: Parser[NonEmptyList[((Float, Float), (Float, Float), (Float, Float))]] =
    (coordinatePairTriple <* commaWsp.?).rep

  val curveToC: Parser[NonEmptyList[C]] =
    ((char('C') ~ wsp.rep0) *> curvetoCoordinateSequence).map {
      case cList: NonEmptyList[((Float, Float), (Float, Float), (Float, Float))] =>
        cList.map((controlPoint1, controlPoint2, finalPoint) =>
          C(controlPoint1._1, controlPoint1._2, controlPoint2._1, controlPoint2._2, finalPoint._1, finalPoint._2))
    }

  val curveToC_ : Parser[NonEmptyList[c_]] =
    ((char('c') ~ wsp.rep0) *> curvetoCoordinateSequence).map {
      case cList: NonEmptyList[((Float, Float), (Float, Float), (Float, Float))] =>
        cList.map((controlPoint1, controlPoint2, finalPoint) =>
          c_(controlPoint1._1, controlPoint1._2, controlPoint2._1, controlPoint2._2, finalPoint._1, finalPoint._2))
    }

  // linetos
  val horizontalLinetoH: Parser[NonEmptyList[H]] = ((char('H') ~ wsp.rep0) *> coordinateSequence).map {
    case hList: NonEmptyList[Float] => hList.map(H.apply)
  }

  val horizontalLinetoH_ : Parser[NonEmptyList[h_]] = ((char('h') ~ wsp.rep0) *> coordinateSequence).map {
    case hList: NonEmptyList[Float] => hList.map(h_.apply)
  }

  val verticalLinetoV: Parser[NonEmptyList[V]] = ((char('V') ~ wsp.rep0) *> coordinateSequence).map {
    case vList: NonEmptyList[Float] => vList.map(V.apply)
  }

  val verticalLinetoV_ : Parser[NonEmptyList[v_]] = ((char('v') ~ wsp.rep0) *> coordinateSequence).map {
    case vList: NonEmptyList[Float] => vList.map(v_.apply)
  }

  val linetoL: Parser[NonEmptyList[L]] = ((char('L') ~ wsp.rep0) *> coordinatePairSequence).map {
    case lList: NonEmptyList[(Float, Float)] => lList.map(L.apply)
  }

  val linetoL_ : Parser[NonEmptyList[l_]] = ((char('l') ~ wsp.rep0) *> coordinatePairSequence).map {
    case lList: NonEmptyList[(Float, Float)] => lList.map(l_.apply)
  }

  // close path
  val closePath: Parser[NonEmptyList[Z]] = charIn("zZ").map(_ => NonEmptyList(Z(), List())) // no need for z_ really

  // moveto
  val movetoM: Parser[NonEmptyList[M]] = ((char('M') ~ wsp.rep0) *> coordinatePairSequence).map {
    case mList: NonEmptyList[(Float, Float)] => mList.map((x: Float, y: Float) => M(x,y))
  }

  val movetoM_ : Parser[NonEmptyList[m_]] = ((char('m') ~ wsp.rep0) *> coordinatePairSequence).map {
    case mList: NonEmptyList[(Float, Float)] => mList.map((x: Float, y: Float) => m_(x,y))
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
    case Nil => NonEmptyList.of(m_(0,0))
  }

  val svgPath: Parser0[NonEmptyList[SvgCommand]] =
    ((wsp.rep0 *> mCombined.?) ~ (mCombined ~ svgCommandRep).?).map {
      case (
        Some( l: NonEmptyList[com.dprl.SvgCommand]),
        Some(ll: cats.data.NonEmptyList[com.dprl.SvgCommand], lll: cats.data.NonEmptyList[com.dprl.SvgCommand] ) ) => l ::: ll ::: lll
      case (
        None,
        Some(ll: cats.data.NonEmptyList[com.dprl.SvgCommand], lll: cats.data.NonEmptyList[com.dprl.SvgCommand] )) => ll ::: lll
      case (
        Some( l: NonEmptyList[com.dprl.SvgCommand]),
        None) => l
      case _ => NonEmptyList(m_(0,0), List())
    }
}
