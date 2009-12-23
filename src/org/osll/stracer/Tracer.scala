package org.osll.stracer

import org.osll.stracer.Utils._

class Tracer(scene: Scene, val options: RenderingOptions) {
  def calcPixel(pixelPos: Tuple2[Int, Int]): Vector = {
    trace(new Ray(CoordinatesOrigin,
    			  getPixelCoordinates(pixelPos) - CoordinatesOrigin))
  }
  
  /**
   * TODO calc absolute pixel coordinates
   */
  def getPixelCoordinates(pixelPos: Tuple2[Int, Int]): Vector =
    CoordinatesOrigin
  
  def trace(ray: Ray): Vector = scene closestIntersectionWith ray match {
    case intersection: ObjectIntersection => shade(ray, intersection)
    case InfinityIntersection => scene.background
  }
  
  /**
   * TODO implement shade algorithm
   */
  def shade(ray: Ray, intersection: ObjectIntersection): Vector = {
    for (light <- scene.lights) {
      var intensity = light.intensity
      val lightDirection = light.pos - intersection.hitPoint
      val lightDistance = lightDirection.length
      
      null
    }
    
    new Vector(0, 0, 0)
  }
  
  def calcLightDirection(light: Light, hitPoint: Vector): Vector = new Vector(0,0,0)
}

abstract class Intersection extends Ordered[Intersection]

case object InfinityIntersection extends Intersection {
  override def compare(that: Intersection): Int  = that match {
    case ObjectIntersection(_, _, _, _) => 1
    case InfinityIntersection => 0
  }
}

case class ObjectIntersection(val hitPoint: Vector,
							  val hitNormal: Vector,
                              val obj: SceneObject,
                              val t: Double) extends Intersection {
  
  override def compare(that: Intersection): Int  = that match {
    case ObjectIntersection(_, _, _, thatT) => t compare thatT
    case InfinityIntersection => -1
  }
}

class Ray(val origin: Vector, val direction: Vector)

class ExtendedRay(origin: Vector, direction: Vector, val obj: SceneObject)
  extends Ray(origin, direction)
