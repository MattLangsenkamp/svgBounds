package com.dprl.model

import cats.data.NonEmptyList
import com.dprl.model.SvgCommand.SvgCommand
import com.dprl.model.Transformation.*

import scala.annotation.targetName

object SvgType {

  sealed trait SvgType {
    def toTag: String
  }

  case class Path(d: NonEmptyList[SvgCommand]) {
    @targetName("transform")
    def *(m: Matrix): Path = Path(d.map{
      svgCommand => svgCommand.transform(m)
    })
  }

  case class Rect() extends SvgType {
    override def toTag: String = ""
  }

  case class Point(x: Double, y: Double)

}
