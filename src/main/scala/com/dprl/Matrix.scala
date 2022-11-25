package com.dprl

import scala.annotation.targetName

case class Matrix(a: Float) {

  @targetName("matMul")
  def * (other: Matrix): Matrix = ???

}
