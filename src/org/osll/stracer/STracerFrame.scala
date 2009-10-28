package org.osll.stracer

import scalala.tensor.{Tensor,Vector};

import java.awt.Color
import java.awt.Image
import java.awt.image.BufferedImage

object STracerFrame {
  
  def toAwtImage(image: Array[Array[Vector]]): Image = {
    val pixels: Array[Int] = new Array(0)
    val awtImage = new BufferedImage(image.size, image(0).size, BufferedImage.TYPE_INT_RGB)
	
    var arrayIndex: Int = 0;
    for (column <- image) {
      for (pixel <- column) {
    	val color: Color = toAwtColor(pixel)
        pixels.update(arrayIndex, ((color.getRed() << 16) | (color.getGreen() << 8) | color.getBlue()) )
        arrayIndex = arrayIndex + 1
      }
    }
    awtImage.setRGB(0, 0, image.size, image(0).size, pixels, 0, image.size);
    
    awtImage
  }
  
  def toAwtColor(color: Vector): Color = {
    new Color(0, 0, 0)
  }

  def main(args: Array[String]) {
    
  }
}
