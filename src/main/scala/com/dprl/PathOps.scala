package com.dprl

import scala.annotation.targetName

object PathOps {
  
  // transform path
  
  @targetName("matMul")
  def *(path: Path, matrix: Matrix): Path = ???
  
  def parsePath(pathString: String): Path = ???
  
  

}
