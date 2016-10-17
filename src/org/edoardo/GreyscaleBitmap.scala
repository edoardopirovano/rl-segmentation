package org.edoardo

import java.awt.Color
import java.awt.image.BufferedImage

/**
  * Created by edoardo on 17/10/16.
  */
class GreyscaleBitmap(val width: Int, val height: Int) {
	val image = new BufferedImage(width, height, BufferedImage.TYPE_USHORT_GRAY)
	
	def setPixel(x: Int, y: Int, g: Int): Unit = {
		image.setRGB(x, y, new Color(g, g, g).getRGB)
	}
	
	def getPixel(x: Int, y: Int): Int = {
		new Color(image.getRGB(x, y)).getRed
	}
}
