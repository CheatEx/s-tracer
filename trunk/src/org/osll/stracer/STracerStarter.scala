package org.osll.stracer

import java.awt.{Color, Image, Graphics}
import java.awt.BorderLayout
import java.awt.image.BufferedImage
import java.io.FileInputStream
import javax.swing.{JPanel, JFrame}

object Starter {
  def main(args: Array[String]) {
    if (args.length != 6) {
      scala.Predef.print("Usage: <stracer-command> width height depth min-weight light-attenuation scene-description")
      return
    }
    val imageWidth = args(0).toInt
    val imageHeight = args(1).toInt
	val depth = args(2).toInt
	val minWeight = args(3).toDouble
	val	lightAttenuation = args(4).toBoolean;
    val descriptionFileName = args(5)
    
    val inputStream = new FileInputStream(descriptionFileName)
    val scene = SceneParser.parseScene(inputStream)
    val options = new RenderingOptions((imageWidth, imageHeight), depth, minWeight, lightAttenuation)
    
    val rawImage: Array[Array[Vector]] = 
      STracer.drawImage(scene, options)
    
    new STracerFrame().show(toAwtImage(rawImage), imageWidth, imageHeight)
    
    0
  }
  
  def toAwtImage(image: Array[Array[Vector]]): Image = {
    val width = image.size
    val height = image(0).size
    val pixels: Array[Int] = new Array(width*height)
    val awtImage = new BufferedImage(width, height, BufferedImage.TYPE_INT_RGB)
	
    var arrayIndex: Int = 0;
    for (column <- image) {
      for (pixel <- column) {
        pixels.update(arrayIndex,  toBufferPixel(pixel))
        arrayIndex = arrayIndex + 1
      }
    }
    awtImage.setRGB(0, 0, image.size, image(0).size, pixels, 0, image.size);
    
    awtImage
  }
  
  def toBufferPixel(color: Vector): Int = {
    val red = toByteComponent(color.x)
    val green = toByteComponent(color.y)
    val blue = toByteComponent(color.z)
    (red << 16) | (green << 8) | blue
  }
  
  def toByteComponent(x: Double): Int = x * 255 intValue
}

class STracerFrame extends JFrame {  
  
  def show(image: Image, width: Int, height: Int) = {
	getContentPane().add(
	  new ImagePanel(image, width, height),
	  BorderLayout.CENTER)
	setSize(width + 6, height + 30)
	setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE)
	setResizable(false)
	setVisible(true)
  }
}

class ImagePanel(val img: Image, val w: Int, val h: Int) extends JPanel {

	override def paint(g: Graphics) = {
		g.drawImage(img, 0, 0, w, h, this);
	}
}
