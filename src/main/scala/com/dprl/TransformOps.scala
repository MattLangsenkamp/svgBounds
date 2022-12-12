package com.dprl

import cats.data.NonEmptyList

object TransformOps {

  def CollapseTransforms(transformList: NonEmptyList[Transform]): Matrix =
    // initialize with identity matrix?
    transformList.foldLeft(Matrix(1, 0, 0, 1, 0, 0))(
      (curMatrix, transform) => transform.toMatrix * curMatrix
    )

}
