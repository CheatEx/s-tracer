package org.osll.stracer

import scalala.tensor.{Tensor,Vector};

class Tracer(scene: Scene, options: RenderingOptions) {
  def calcPixel(pixelPos: Tuple2[Int, Int]): Vector = {
    Vector()
  }
  
  def trace(ray: ExtendedRay): Vector = {
    val intersection = scene intersectClosest ray
    shade(ray, intersection)
  }
  
  def shade(ray: ExtendedRay, intersection: Intersection): Vector = {
    Vector(0, 0, 0)
  }
}

abstract class Intersection extends Ordered[Intersection]

case object InfinityIntersection extends Intersection {
  override def compare(that: Intersection): Int  = that match {
    case ObjectIntersection(_, _, _, thatT) => 1
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