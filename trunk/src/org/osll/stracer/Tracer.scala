package org.osll.stracer

import scalala.tensor.Vector;

import org.osll.stracer.Utils._

class Tracer(scene: Scene, val options: RenderingOptions) {
  def calcPixel(pixelPos: Tuple2[Int, Int]): Vector = {
    Vector(0, 0, 0)
  }
  
  def trace(ray: Ray): Vector = scene closestIntersection ray match {
      case intersection: ObjectIntersection => shade(ray, intersection)
      case InfinityIntersection => scene.background
    }
  
  def shade(ray: Ray, intersection: ObjectIntersection): Vector = {
    for (light <- scene.lights) {
      var intensity = light.intensity
      val lightDirection = Vector(light.pos(0) - intersection.hitPoint(0),
      							  light.pos(1) - intersection.hitPoint(1),
      							  light.pos(2) - intersection.hitPoint(2))
      val lightDistance = module(lightDirection)
      
      null
    }
    
    Vector(0, 0, 0)
  }
  
  def calcLightDirection(light: Light, hitPoint: Vector): Vector = Vector()
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