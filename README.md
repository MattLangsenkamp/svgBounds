# SVGBounds
SVGBounds was created to extract bounding boxes from the elements in SVGs, including path elements.

## Installation

## Usage

SVGBounds contains methods to load, parse, and visualize the bounding boxes. An example of the usage of these utilities is shown below.
This default parser is written specifically to deal with SVGs rendered using MathJax
```scala
import scala.io.Source
import scala.xml.{Node, XML}

val fileContents = Source.fromFile("path/to/svg.svg").getLines.mkString
val bboxList = defaultParse(fileContents)
val root = XML.loadString(fileContents)
XML.save("test.svg", Visualize.addBoundingBoxes(root, bboxList))
```

