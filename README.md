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
The purpose of this library is to provide functionality needed to parse SVG paths, transform SVG paths, and compute the bounding box of SVG paths.

Much of this work has been derived straight from the [SVG path spec](https://www.w3.org/TR/SVG/paths.html) and the [SVG transform spec](https://www.w3.org/TR/SVG/coords.html) 

This library is geared towards extracting bounding box information from SVGs and does not implement all the functionality within the SVG specification.

