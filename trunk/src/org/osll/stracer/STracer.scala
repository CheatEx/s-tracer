package org.osll.stracer

import scala.List._
import scala.collection.mutable.ArrayBuffer

import scalala.Scalala._;
import scalala.tensor.{Tensor,Vector};

class RenderingOptions(
    resolution: Tuple2[Int, Int], depth: Int, minWeight: Double, lightAttenuation: Boolean) {
	
  def width = resolution._1
  def height = resolution._2
}

class STracer(scene: Scene, options: RenderingOptions) {
  
  def drawImage(): Array[Array[Vector]] = {
	val image = new Array[Array[Vector]](options.width, options.height)
	for (i <- range(0, options.width); j <- range(0, options.height)) {
	  
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
    val map = new Array[Array[Vector]](image.length, image(0).length)
    for (i <- range(0, image.length); j <- range(0, image(0).length)) {
      map(i).update(j, func(image(i)(j)))
    }
    map
  } 
  
  def applyForPixels(func: (Vector)=> Unit, image: Array[Array[Vector]]): Unit = {
    for (column <- image; pixel <- column) {
      func(pixel)
    }
  }
  
  def applyForComponents(func: (Double)=> Unit, image: Array[Array[Vector]]): Unit =
    applyForPixels(pixel => for (component <- pixel) func(component._2) , image)
}
