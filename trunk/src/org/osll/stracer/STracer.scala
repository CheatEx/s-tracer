package org.osll.stracer

import java.io.InputStream

import scala.List._
import scala.io.Source
import scala.collection.mutable.ArrayBuffer

import scalala.Scalala._;
import scalala.tensor.{Tensor,Vector};

class RenderingOptions(
    resolution: Tuple2[Int, Int], depth: Int, minWeight: Double, lightAttenuation: Boolean) {
	
  def width = resolution._1
  def height = resolution._2
}

object STracer {
  
  def drawImage(scene: Scene, options: RenderingOptions): Array[Array[Vector]] = {
	val image = new Array[Array[Vector]](options.width, options.height)
	val tracer = new Tracer(scene, options)
	for (i <- range(0, options.width); j <- range(0, options.height)) {
	  image(i)(j) = tracer.calcPixel(i, j)
	}
	
	image
  }
  
  def normalize(image: Array[Array[Vector]]) = {
    var maxValue = 1d;
	var minValue = 0d;
    applyForComponents(
      component => {
        if (component > maxValue) maxValue = component
        if (component < minValue) minValue = component
      }, image)
    
    val normalisation: Double = - minValue + maxValue
    mapImage(
      pixel => {
        (pixel - minValue) / normalisation value
      }, image)
  }
  
  def mapImage(func: (Vector)=>Vector, image: Array[Array[Vector]]): Array[Array[Vector]] = {
    image map
      (column => column map
        (pixel => func(pixel)))
  } 
  
  def applyForComponents(func: (Double)=> Unit, image: Array[Array[Vector]]): Unit =
    applyForPixels(pixel => for (component <- pixel) func(component._2) , image)
  
  def applyForPixels(func: (Vector)=> Unit, image: Array[Array[Vector]]): Unit = {
    for (column <- image; pixel <- column) func(pixel)
  }
}

object SceneParser {
  def parseScene(sceneDescription: InputStream): Scene = 
    new Scene(null, List(), List(), Vector(0,0,0), Vector(0,0,0))
}