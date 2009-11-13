package org.osll.stracer

import scalala.tensor.Vector;

class Tracer(scene: Scene, val options: RenderingOptions) {
  def calcPixel(pixelPos: Tuple2[Int, Int]): Vector = {
    Vector(0, 0, 0)
  }
  
  def trace(ray: Ray): Vector = scene closestIntersection ray match {
      case intersection: ObjectIntersection => shade(ray, intersection)
      case InfinityIntersection => scene.background
    }
  
  def shade(ray: Ray, intersection: Intersection): Vector = {
    val color = Vector(0,0,0)
    val rColor = Vector(0,0,0)
    val tColor = Vector(0,0,0)
    
    
    
    Vector(0, 0, 0)
  }
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