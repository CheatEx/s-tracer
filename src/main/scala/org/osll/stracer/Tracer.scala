package org.osll.stracer

import scala.Math._

import org.osll.stracer.Utils._

class Tracer(scene: Scene, val options: RenderingOptions) {
  val screenDistance = 1
  val screen = new Screen(scene.camera, screenDistance, options.width, options.height)
  
  def calcPixel(pixelPos: Tuple2[Int, Int]): Vector = {
    trace(new Ray(CoordinatesOrigin,
    			  			screen.getPixelCoordinates(pixelPos) - CoordinatesOrigin))
  }

  def trace(ray: Ray): Vector = scene closestIntersectionWith ray match {
    case intersection: ObjectIntersection => shade(ray, intersection)
    case InfinityIntersection => scene.background
  }

  /**
   * TODO implement shade algorithm
   */
  def shade(ray: Ray, intersection: ObjectIntersection): Vector = {
    var color = ambientColor(intersection)
    
    for (light <- scene.lights) {
      //there is direction to light from hit point
      val lightDirection = light.pos - intersection.hitPoint
      val lightDistance = lightDirection.length
      var intensity = light.intensity  
      
      if (light.isInstanceOf[SpotLight]) {
        //this is direction of spot
        val lightDirection = -light.asInstanceOf[SpotLight].at
        val hitPointDirection = light.pos - intersection.hitPoint normalize
        
        val spotAngle = light.asInstanceOf[SpotLight].angle
        val hitPointAngle = acos(lightDirection ** hitPointDirection)
        if (hitPointAngle > spotAngle || hitPointAngle < 0) {
					//TODO skip this light
				}
        val falloff = 1 - pow(hitPointAngle / spotAngle, 2);
        intensity *= falloff
      }
      
      if (options.lightAttenuation) {
        val falloff = 1.0 - Math.pow(lightDistance, 2);
				intensity *= falloff;
      }
      
      val viewingDirection = -ray.direction
      val neh = lightDirection + viewingDirection
      
      val nl = intersection.hitNormal ** lightDirection
      var nh = intersection.hitNormal ** neh
      
      if (nl > 0) {
        val shadowRayDirection = light.pos - intersection.hitPoint normalize
        val shadowRayOrigin = shadowRayDirection * 1e-3 + intersection.hitPoint
        val shadowRay = new Ray(shadowRayOrigin, shadowRayDirection)
        val shadowAttenuation = computeShadowAttenuation(shadowRay, lightDistance);
        
        color += intensity * intersection.obj.material.Kd * nl * shadowAttenuation
        nh = pow(nh, intersection.obj.material.alpha);
        color += intensity * intersection.obj.material.Ks * nh * shadowAttenuation
      }
    }
    
    color *= intersection.obj.material.lC
    color += trace(computeReflectedRay)*intersection.obj.material.rC
    color += trace(computeTransmittedRay)*intersection.obj.material.tC

    color
  }
  
  def computeReflectedRay: ExtendedRay = null
  def computeTransmittedRay: ExtendedRay = null
  
  def computeShadowAttenuation(shadowRay: Ray, lightDistance: Double): Double = {
    var shadowAttenuation = 1.0
    for (intersection <- scene.allIntersectionsWith(shadowRay)) intersection match {
        case ObjectIntersection(_, _, obj, t) =>
          if ((shadowRay.direction * t + shadowRay.origin length) < lightDistance) {
        	  shadowAttenuation *= obj.material.tC
          }
        case InfinityIntersection => {}
      }
    shadowAttenuation
  }
  
  def ambientColor(intersection: ObjectIntersection): Vector =
    scene.ambientLight * intersection.obj.material.Ka
}

class Screen(camera: Camera, screenDistance: Double,
             verticalResolution: Double, horisontalResolution: Double) {

  def getPixelCoordinates(pixelPos: Tuple2[Int, Int]): Vector = 
    topBottom + screen2Absolute(d*pixelPos._1, d*pixelPos._2)

  def screen2Absolute(x: Double, y: Double) = nx*x + ny*y

  val topBottom: Vector =
    screenCenter + screen2Absolute(-width/2, -height/2)

  val screenCenter: Vector =
    camera.at * screenDistance / camera.at.length

  val width: Double = tan(camera.viewport/2)*screenDistance*2
  val height: Double = verticalResolution*d
  val d: Double = width/horisontalResolution
  /**
   * TODO find screen coordinate system
   */
  val nx: Vector = CoordinatesOrigin
  val ny: Vector = CoordinatesOrigin
}

abstract sealed class Intersection extends Ordered[Intersection]

case object InfinityIntersection extends Intersection {
  override def compare(that: Intersection): Int  = that match {
    case ObjectIntersection(_, _, _, _) => 1
    case InfinityIntersection => 0
  }
}

case class ObjectIntersection(val hitPoint: Vector, val hitNormal: Vector,
                              val obj: MaterialObject, val t: Double)
    extends Intersection {
  
  override def compare(that: Intersection): Int  = that match {
    case ObjectIntersection(_, _, _, thatT) => t compare thatT
    case InfinityIntersection => -1
  }
}

class Ray(val origin: Vector, val direction: Vector)

class ExtendedRay(origin: Vector, direction: Vector, val obj: SceneObject)
  extends Ray(origin, direction)
