package com.dprl

object Main {

  def main(args: Array[String]): Unit = {
    println(Parse.number.parse("5343.846767f-1"))
    println(Parse.number.parse("5343e-1"))
    println(Parse.coordinate.parse("123E-1"))
    println(Parse.coordinatePair.parse("-123, 435"))
    println(Parse.coordinateSequence.parse("44, 325, 434, 4343,43"))
    println(Parse.coordinateSequence.parse("44"))
  }

}
