# SVGBounds
SVGBounds was created to extract bounding boxes from the elements in SVGs, including path elements.
Much of this work has been derived straight from the [SVG path spec](https://www.w3.org/TR/SVG/paths.html) and the [SVG transform spec](https://www.w3.org/TR/SVG/coords.html)

## Installation

To run this code you will need a Java runtime installed, Scala installed and sbt installed. To manage these 
requirements we recommend using [sdkman](https://sdkman.io/)

run the following commands, setting each as the default when prompted

```bash
sdk install scala 3.2.2
sdk install sbt 1.8.2
sbt compile
sbt run
```


## Use in another JVM Project



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

## Limitations
The purpose of this library is to provide functionality needed to parse SVG paths, transform SVG paths, and compute the bounding box of SVG paths.
This library does not implement all the functionality within the SVG specification and currently there are no plans to implement these functionalities.

Currently, there are issues generating tight bounding boxes for elliptical arcs. 
Some SVG types such as circles and ellipses are currently not implemented. We intend to address these limitations at some point. 


