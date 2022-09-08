package com.dprl

import org.locationtech.jts.geom.{Geometry, LineString, Polygon, Point}

object Extract {

  //  returns (minX: Float, minY: Float, maxX: Float, maxY:Float)
  def fromPath(f: Polygon): (Float, Float, Float, Float) = f.getEnvelope match
    case p: Polygon => ???
    case l: LineString => ???
    case p: Point => ???

  def fromRect(): (Float, Float, Float, Float) = ???

  def fromCircle(): (Float, Float, Float, Float) = ???

}
