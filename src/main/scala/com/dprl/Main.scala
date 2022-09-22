package com.dprl
import cats.data.NonEmptyList

object Main {

  def main(args: Array[String]): Unit = {
    Parse.svgCommand.rep0.map {
      case ::(head, next) => {
        next.foldLeft(head)((svgCommandList, n) => {
          svgCommandList ::: n
        }
        )
      }

      case Nil => NonEmptyList.of(m_(0,0))
    }.parse("M 1, 2 V 1")


  }

}
