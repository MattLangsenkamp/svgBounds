package com.dprl
import scala.math.{max, min, pow, sqrt}

object CurveUtils {

  // solution adapted from https://github.com/meerk40t/svgelements/blob/master/svgelements/svgelements.py
  def quadraticBezierBoundingBox(sPointX: Double, sPointY: Double,
                                 cPointX: Double, cPointY: Double,
                                 endPointX: Double, endPointY: Double): Bounds = {

    def pointX(t: Double) = (pow(1-t, 2)*sPointX) + (2*(1-t)*t*cPointX) + pow(t,2)*endPointX
    def pointY(t: Double) = (pow(1-t, 2)*sPointY) + (2*(1-t)*t*cPointY) + pow(t,2)*endPointY
    val nX = sPointX-cPointX
    val dX = sPointX-2*cPointX + endPointX
    val tX = if (dX != 0) {
      nX/dX
    } else 0.5

    val xValues = if (0<tX && tX<1)
      List(sPointX, endPointX, pointX(tX))
    else
      List(sPointX, endPointX)

    val nY = sPointY-cPointY
    val dY = sPointY-2*cPointY+ endPointY
    val tY = if (dY != 0) {
      nY/dY
    } else 0.5

    val yValues = if (0<tY && tY<1)
      List(sPointY, endPointY, pointY(tY))
    else
      List(sPointY, endPointY)

    Bounds(xValues.min, yValues.min, xValues.max, yValues.max)
  }

  // solution adapted from https://eliot-jones.com/2019/12/cubic-bezier-curve-bounding-boxes
  def cubicBezierBoundingBox(sPointX: Double, sPointY: Double,
                             cPoint1X: Double, cPoint1Y: Double,
                             cPoint2X: Double, cPoint2Y: Double,
                             endPointX: Double, endPointY: Double): Bounds = {
    val (solX1, solX2) = solveQuadratic(sPointX, cPoint1X, cPoint2X, endPointX)
    val (solY1, solY2) = solveQuadratic(sPointY, cPoint1Y, cPoint2Y, endPointY)

    var minX = min(sPointX, endPointX)
    var maxX = max(sPointX, endPointX)

    solX1.foreach(v => {
      minX = min(minX, v)
      maxX = max(maxX, v)
    })

    solX2.foreach(v => {
      minX = min(minX, v)
      maxX = max(maxX, v)
    })

    var minY = min(sPointY, endPointY)
    var maxY = max(sPointY, endPointY)

    solY1.foreach(v => {
      minY = min(minY, v)
      maxY = max(maxY, v)
    })

    solY2.foreach(v => {
      minY = min(minY, v)
      maxY = max(maxY, v)
    })

    Bounds(minX, minY, maxX, maxY)
  }

  def solveQuadratic(p0: Double, p1: Double, p2: Double, p3: Double): (Option[Double], Option[Double]) = {
    val i = p1 -p0
    val j = p2 - p1
    val k = p3 - p2

    val a = (3*i) - (6*j) + (3*k)
    val b = (6*j) - (6*i)
    val c = (3*i)

    val sqrtPart = (b*b) - (4*a*c)
    if (!(sqrtPart>=0)) {
      (None, None)
    } else {
      val t1 = (-b + sqrt(sqrtPart)) / (2 * a);
      val t2 = (-b - sqrt(sqrtPart)) / (2 * a);

      val s1 = if (t1 >= 0 && t1 <= 1) {
        Some(getBezierValueForT(t1, p0, p1, p2, p3))
      } else {
        None
      }

      val s2 = if (t2 >= 0 && t2 <= 1) {
        Some(getBezierValueForT(t2, p0, p1, p2, p3))
      } else {
        None
      }

      (s1, s2);
    }
  }

  def getBezierValueForT(t: Double, p0: Double, p1: Double, p2: Double, p3: Double): Double = {
    val oneMinusT = 1 - t

    (pow(oneMinusT, 3) * p0)
      + (3 * pow(oneMinusT, 2) * t * p1)
      + (3 * oneMinusT * pow(t, 2) * p2)
      + (pow(t, 3) * p3)
  }
  def ellipticalArcBoundingBox(
                                curX: Double, curY:Double,
                                rx: Double, ry: Double,
                                xAxisRotation: Double,
                                largeArcFlag: Short,
                                sweepFlag: Short,
                                endX: Double, endY: Double): Bounds = {
    if (sweepFlag == 0) Bounds(curX, curY, endX, endY)
    else {
      val phi = xAxisRotation.toRadians
      xAxisRotation
    }

  }

  // solution adapted from http://fridrich.blogspot.com/2011/06/bounding-box-of-svg-elliptical-arc.html
  def ellipticalArcBoundingBox2(
                                curX: Double, curY:Double,
                                rx: Double, ry: Double,
                                xAxisRotation: Double,
                                largeArcFlag: Short,
                                sweepFlag: Short,
                                endX: Double, endY: Double): Bounds = {
    var rX = if(rx < 0.0) -rx else rx
    var rY = if(ry < 0.0) -ry else ry

    if(rX == 0 || rY == 0) {
      Bounds(math.min(curX, endX), math.min(curY, endY), math.max(curX, endX), math.max(curY, endY))
    } else {
      val x1Prime = math.cos(xAxisRotation)*(curX-endX)/2 + math.sin(xAxisRotation)*(curY-endY)/2
      val y1Prime = -math.sin(xAxisRotation)*(curX-endX)/2 + math.cos(xAxisRotation)*(curY-endY)/2

      val radicant: Double = (rX*rX*rY*rY - rX*rX*y1Prime*y1Prime - rY*rY*x1Prime*x1Prime)/
        (rX*rX*y1Prime*y1Prime +rY*rY*x1Prime*x1Prime)

      var cXPrime: Double = 0.0
      var cYPrime: Double = 0.0
      if (radicant < 0.0) {
        val ratio = rX/rY
        val nRadicant = y1Prime*y1Prime + x1Prime*x1Prime/(ratio*ratio)
        if (nRadicant < 0.0) {
          return Bounds(math.min(curX, endX), math.min(curY, endY), math.max(curX, endX), math.max(curY, endY))
        }
        rY=math.sqrt(nRadicant)
        rX=ratio*rY
      } else {
        val factor = (if (largeArcFlag == sweepFlag) -1.0 else 1)*math.sqrt(radicant)
        cXPrime = factor*rX*y1Prime/rY
        cYPrime = -factor*rY*x1Prime/rX
      }

      val cX = cXPrime*math.cos(xAxisRotation) - cYPrime*math.sin(xAxisRotation) + (curX+endX)/2
      val cY = cYPrime*math.sin(xAxisRotation) - cYPrime*math.cos(xAxisRotation) + (curY+endY)/2
      var (xMin: Double, xMax: Double, yMin: Double, yMax: Double) = (0.0, 0.0, 0.0, 0.0)
      val (tXMin: Double, tXMax: Double, tYMin: Double, tYMax: Double) = if (xAxisRotation == 0 || xAxisRotation == math.Pi) {
        xMin = cX - rX
        xMax = cX + rX
        yMin = cY - rY
        yMax = cY + rY
        (getAngle(-rX, -0), getAngle(rx, 0), getAngle(0, -rY), getAngle(0, rY))
      } else if(xAxisRotation == math.Pi/2 || xAxisRotation == 3*math.Pi/2) {
        xMin = cX - rY
        xMax = cX + rY
        yMin = cY - rX
        yMax = cY + rX
        (getAngle(-rY, -0), getAngle(rY, 0), getAngle(0, -rX), getAngle(0, rX))
      } else {
        var tXMinTemp = -math.atan(rY*math.tan(xAxisRotation)/rX)
        var tXMaxTemp = math.Pi - math.atan(rY*math.tan(xAxisRotation)/rX)
        xMin = cX + rX*math.cos(tXMinTemp)*math.cos(xAxisRotation) - rY*math.sin(tXMinTemp)*math.sin(xAxisRotation)
        xMax = cX + rX*math.cos(tXMaxTemp)*math.cos(xAxisRotation) - rY*math.sin(tXMaxTemp)*math.sin(xAxisRotation)
        if (xMin > xMax) {
          val xMinHold = xMin
          xMin = xMax
          xMax = xMinHold

          val tXMinHold = tXMinTemp
          tXMinTemp = tXMaxTemp
          tXMaxTemp = tXMinHold

        }

        var tmpY = cY + rX*math.cos(tXMinTemp)*math.sin(xAxisRotation) + rY*math.sin(tXMinTemp)*math.cos(xAxisRotation)
        tXMinTemp = getAngle(xMin - cX, tmpY - cY)
        tmpY = cY + rX*math.cos(tXMaxTemp)*math.sin(xAxisRotation) + rY*math.sin(tXMaxTemp)*math.cos(xAxisRotation)
        tXMaxTemp = getAngle(xMax - cX, tmpY - cY)

        var tYMinTemp = math.atan(rY/(math.tan(xAxisRotation)*rX))
        var tYMaxTemp = math.atan(rY/(math.tan(xAxisRotation)*rx)) + math.Pi
        yMin = cY + rX*math.cos(tYMinTemp)*math.sin(xAxisRotation) + rY*math.sin(tYMinTemp)*math.cos(xAxisRotation)
        yMax = cY + rX*math.cos(tYMaxTemp)*math.sin(xAxisRotation) + rY*math.sin(tYMaxTemp)*math.cos(xAxisRotation)

        if (yMin > yMax) {
          val yMinHold = yMin
          yMin = yMax
          yMax = yMinHold

          val tYMinHold = tYMinTemp
          tYMinTemp = tYMaxTemp
          tYMaxTemp = tYMinHold
        }

        var tmpX = cX + rX*math.cos(tYMinTemp)*math.cos(xAxisRotation) - rY*math.sin(tYMinTemp)*math.sin(xAxisRotation)
        tYMinTemp = getAngle(tmpX-cX, yMin-cY)
        tmpX = cX + rX*math.cos(tYMaxTemp)*math.cos(xAxisRotation) - rY*math.sin(tYMaxTemp)*math.sin(xAxisRotation)
        tYMaxTemp = getAngle(tmpX-cX, yMin-cY)
        (tXMinTemp, tYMinTemp, tXMaxTemp, tYMaxTemp)
      }

      var angle1 = getAngle(curX - cX, curY - cY)
      var angle2 = getAngle(endX - cX, endY - cY)

      if (sweepFlag != 1) {
        val angle1Hold = angle1
        angle1 = angle2
        angle2 = angle1Hold
      }

      var otherArc = false
      if(angle1 > angle2) {
        val angle1Hold = angle1
        angle1 = angle2
        angle2 = angle1Hold
        otherArc = true
      }

      if ((!otherArc && (angle1 > tXMin || angle2 < tXMin)) || (otherArc && !(angle1 > tXMin || angle2 < tXMin)))
        xMin = min(curX, endX)

      if ((!otherArc && (angle1 > tXMax || angle2 < tXMax)) || (otherArc && !(angle1 > tXMax || angle2 < tXMax)))
        xMax = max(curX, endX)

      if ((!otherArc && (angle1 > tYMin || angle2 < tYMin)) || (otherArc && !(angle1 > tYMin || angle2 < tYMin)))
        yMin = min(curY, endY);

      if ((!otherArc && (angle1 > tYMax || angle2 < tYMax)) || (otherArc && !(angle1 > tYMax || angle2 < tYMax)))
        yMax = max(curY, endY)

      Bounds(xMin, xMin, xMax, yMax)
    }
  }


  def getAngle(bX: Double, bY: Double): Double =
    (2*math.Pi + (if (bY > 0.0) 1 else -1) * math.acos(bX/math.sqrt(bX*bX+bY*bY))) % (2*math.Pi)
}
