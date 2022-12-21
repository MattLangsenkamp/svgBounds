package com.dprl
import cats.data.NonEmptyList
import com.dprl.SvgParse.defaultParse

import scala.io.Source
import scala.xml.{Node, XML}
object Main {

  def main(args: Array[String]): Unit = {
    val t = "x+y+z"
    val fileContents = Source.fromResource(s"svg/$t.svg").getLines.mkString
    val ok =  time {
      val fileContents = Source.fromResource(s"svg/$t.svg").getLines.mkString
      defaultParse(fileContents)}("parse")
    println(ok)
    val justBBoxes = ok._1.map((b, _) => b)
    val root = XML.loadString(fileContents)
    XML.save(s"test$t.svg", Visualize.addBoundingBoxes(root, justBBoxes))

  }

  def time[R](block: => R)(message: String): R = {
    val t0 = System.nanoTime()
    val result = block // call-by-name
    val t1 = System.nanoTime()
    println(s"Elapsed time to complete $message: " + (t1 - t0)/1e9 + "s")
    result
  }

}
