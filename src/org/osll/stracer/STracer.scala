package org.osll.stracer

import java.io.{InputStream, Reader, InputStreamReader, BufferedReader}
import java.util.StringTokenizer

import scala.List._
import scala.io.Source
import scala.collection.mutable.ArrayBuffer

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
        (pixel - minValue) / normalisation
      }, image)
  }
  
  def mapImage(func: (Vector)=>Vector, image: Array[Array[Vector]]): Array[Array[Vector]] = {
    image map
      (column => column map func)
  } 
  
  def applyForComponents(func: (Double)=> Unit, image: Array[Array[Vector]]): Unit =
    for (column <- image;
         pixel <- column;
    		 component <- pixel)
    	func(component)
}

object SceneParser {
  def parseScene(input: InputStream): Scene = {
    val reader = new BufferedReader(new InputStreamReader(input))
    
    val background = readVector(reader)
    val ambientLight = readVector(reader)
    val camera = readCamera(reader)
    val lights = readLights(reader)
    val objects = readObjects(reader)
    
	new Scene(camera, objects, lights, background, ambientLight)
  }
  
  def readCamera(reader: BufferedReader): Camera = {
    val at = readVector(reader)
    val up = readVector(reader)
    val viewport = reader readLine() trim() toDouble
    
    new Camera(at, up, viewport)
  }
  
  def readLights(reader: BufferedReader): List[Light] = 
    readObjectGroup(reader, readPointLight):::readObjectGroup(reader, readSpotLight)
  
  def readObjects(reader: BufferedReader): List[MaterialObject] =
    readObjectGroup(reader, readSphere):::readObjectGroup(reader, readPolyObject)
  
  def readObjectGroup[ObjectType](reader: BufferedReader,
                                  readFunction: BufferedReader => ObjectType): List[ObjectType] = {
    reader readLine
    val objectsCount = reader readLine() trim() toInt;
    for (i <- range(0, objectsCount)) yield readFunction(reader)
  }

  def readPointLight(reader: BufferedReader): PointLight = {
	  val name = reader readLine() trim()
	  val position = readVector(reader)
	  val intensity = readVector(reader)
	    
	  new PointLight(position, intensity)
  }
  
  def readSpotLight(reader: BufferedReader): SpotLight = {
    val name = reader readLine() trim()
    val position = readVector(reader)
    val at = readVector(reader)
    val angle = reader readLine() trim() toDouble
    val intensity = readVector(reader)
    
    new SpotLight(position, at, angle, intensity)
  }
  
  def readSphere(reader: BufferedReader): Sphere = {
    val name = reader readLine() trim()
    val position = readVector(reader)
    val radius = reader readLine() trim() toDouble
    val material = readMaterial(reader)
    
    new Sphere(position, material, radius)
  }
  
  def readPolyObject(reader: BufferedReader): Polygon = {
    val material = readMaterial(reader)
    val vertexes = readObjectGroup(reader, readVector)
    val triangles = readObjectGroup(reader, readTriangle)
    new Polygon(material, vertexes, triangles)
  }
  
  def readMaterial(reader: BufferedReader): Material = {
    null
  }
  
  def readTriangle(reader: BufferedReader): Tuple3[Int, Int, Int] = {
    val tokenizer = new StringTokenizer(reader.readLine)
    Tuple3(tokenizer.nextToken().toInt,
           tokenizer.nextToken().toInt,
           tokenizer.nextToken().toInt)
  }
  
  def readVector(reader: BufferedReader): Vector = {
    val tokenizer = new StringTokenizer(reader.readLine)
    new Vector(tokenizer.nextToken().toDouble,
           tokenizer.nextToken().toDouble,
           tokenizer.nextToken().toDouble)
  }
}
