package com.dprl

import cats.data.NonEmptyList
import com.dprl.model.SvgCommand.*
import com.dprl.model.Transformation.*
import com.dprl.model.SvgType.Path

import scala.annotation.targetName

object PathOps {

  def CollapseTransforms(transformList: NonEmptyList[Transformation]): Matrix =
  // initialize with identity matrix?
    transformList.foldLeft(Matrix(1, 0, 0, 1, 0, 0))(
      (curMatrix, transform) => transform.toMatrix * curMatrix
    )

  def parsePath(pathString: String): Path = ???

  def getBounds(path: Path): Bounds = ???

}
