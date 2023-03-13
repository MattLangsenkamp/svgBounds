package org.dprl.svgbounds

import cats.data.NonEmptyList
import cats.parse.Parser.{char, charIn, string}
import cats.parse.{Parser, Parser0}
import org.dprl.svgbounds.PathParse
import org.dprl.svgbounds.model.Transformation.*

// derived from https://www.w3.org/TR/css-transforms-1/#svg-syntax
object TransformParse {
  val leftParenthesis: Parser[Unit] = char('(')
  val rightParenthesis: Parser[Unit] = char(')')
  val wsp: Parser[Char] = PathParse.wsp
  val comma: Parser[Unit] = PathParse.comma
  val commaWsp: Parser[String] = PathParse.commaWsp
  val number: Parser[Double] = PathParse.number.map(_.toDouble)
  val coordinate: Parser[Double] = PathParse.coordinate

  val translate: Parser[Translate] = ((string("translate") ~ wsp.rep0 ~ leftParenthesis ~ wsp.rep0)
    *> coordinate ~ (commaWsp *> coordinate).backtrack.? <* (wsp.rep0 <* rightParenthesis))
    .map((x: Double, y: Option[Double]) => Translate(x, y))

  val scale: Parser[Scale] = ((string("scale") ~ wsp.rep0 ~ leftParenthesis ~ wsp.rep0)
    *> coordinate ~ (commaWsp *> coordinate).backtrack.? <* (wsp.rep0 ~ rightParenthesis))
    .map((x: Double, y: Option[Double]) => Scale(x, y))

  val rotate: Parser[Rotate] =
    ((string("rotate") ~ wsp.rep0 ~ leftParenthesis ~ wsp.rep0)
      *> coordinate ~ (commaWsp *> coordinate ~ (commaWsp *> coordinate)).backtrack.?
      <* (wsp.rep0 ~ rightParenthesis)).map((a: Double, xY: Option[(Double, Double)]) => Rotate(a, xY))

  val skewX: Parser[SkewX] = ((string("skewX") ~ wsp.rep0 ~ leftParenthesis ~ wsp.rep0)
    *> coordinate <*
    (wsp.rep0 ~ rightParenthesis)).map(SkewX.apply)

  val skewY: Parser[SkewY] = ((string("skewY") ~ wsp.rep0 ~ leftParenthesis ~ wsp.rep0)
    *> coordinate <*
    (wsp.rep0 ~ rightParenthesis)).map(SkewY.apply)

  val matrix: Parser[Matrix] = ((string("matrix") ~ wsp.rep0 ~ leftParenthesis ~ wsp.rep0)
    *> (coordinate <* commaWsp)
    ~ (coordinate <* commaWsp)
    ~ (coordinate <* commaWsp)
    ~ (coordinate <* commaWsp)
    ~ (coordinate <* commaWsp)
    ~ (coordinate <* (wsp.rep0 ~ rightParenthesis)))
    .map {
      case (((((a: Double, b: Double), c: Double), d: Double), e: Double), f: Double) => Matrix(a, b, c, d, e, f)
    }
  val transform: Parser[Transformation] = Parser.oneOf(List(
    translate,
    scale,
    rotate,
    skewX,
    skewY,
    matrix
  ))
  val transforms: Parser[NonEmptyList[Transformation]] = (transform <* commaWsp.rep0).rep
  val transformList: Parser[NonEmptyList[Transformation]] = (wsp.rep *> transforms <* wsp.rep0) | (transforms <* wsp.rep0)
}
