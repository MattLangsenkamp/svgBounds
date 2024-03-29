package org.dprl.svgbounds

import cats.data.NonEmptyList
import org.dprl.svgbounds.Visualize
import SvgParse.defaultParse

import scala.io.Source
import scala.xml.{Node, XML}
object Main {

  def main(args: Array[String]): Unit = {

    println("SvgBounds main script")
    time {val t = "updownup"
    val fileContents = Source.fromResource(s"svg/$t.svg").getLines.mkString
    val ok = defaultParse(fileContents)
    val justBBoxes = ok._1.map((b, _) => b)
    val root = XML.loadString(fileContents)
    XML.save(s"test$t.svg", Visualize.addBoundingBoxes(root, justBBoxes))} ("bounding box generation with default parser")

  }

  def time[R](block: => R)(message: String): R = {
    val t0 = System.nanoTime()
    val result = block // call-by-name
    val t1 = System.nanoTime()
    val t = (t1 - t0)/1e9
    if (t>0.00001) println(s"Elapsed time to complete $message: " + t + "s")
    result
  }
}
