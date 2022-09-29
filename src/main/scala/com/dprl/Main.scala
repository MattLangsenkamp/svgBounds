package com.dprl
import cats.data.NonEmptyList
import org.jsoup.Jsoup

import scala.xml.{XML, Node}
object Main {

  def main(args: Array[String]): Unit = {
    val lolz =
      """<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 112 90">
        |<circle ten="11">
        | <fake/>
        |</circle>
        |<circle ten="11">
        | <stake/>
        |</circle>
        | <bob>
        |   <bob>
        |   </bob>
        | </bob>
        | <bob>
        | </bob>
        | <bob>
        | </bob>
        |</svg>
        |""".stripMargin

    val hmm = XML.loadString(lolz)
    val circleChild = (hmm \ "circle").head
    (hmm \ "circle").foreach((n: Node)  => {
      val stakeOrFake = n \ "fake"
      println(stakeOrFake)
    })
    val ten = circleChild.attribute("ten")
    println(ten.get.head.mkString)
    println(circleChild.attributes.asAttrMap("ten"))
    val fake = circleChild \ "fake"
    println(fake)

  }

}
