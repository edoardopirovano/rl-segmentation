package org.edoardo

import java.awt.image.BufferedImage
import java.awt.{Color, Graphics}


class RgbBitmap(val width: Int, val height: Int) {
	val image = new BufferedImage(width, height, BufferedImage.TYPE_3BYTE_BGR)
	
	val xGradient = new GreyscaleBitmap(width, height)
	val yGradient = new GreyscaleBitmap(width, height)
	val greyScale = new GreyscaleBitmap(width, height)
	
	def getState(x: Int, y: Int): PixelInfo = {
		PixelInfo(xGradient.getPixel(x, y), yGradient.getPixel(x, y), greyScale.getPixel(x, y))
	}
	
	def fill(c: Color): Unit = {
		val g: Graphics = image.getGraphics
		g.setColor(c)
		g.fillRect(0, 0, width, height)
	}
	
	def setPixel(x: Int, y: Int, c: Color): Unit = image.setRGB(x, y, c.getRGB)
	
	def getPixel(x: Int, y: Int) = new Color(image.getRGB(x, y))
	
	def contains(x: Int, y: Int): Boolean = x >= 0 && y >= 0 && x < width && y < height
	
	def getGrey(x: Int, y: Int): Int = {
		if (contains(x, y)) greyScale.getPixel(x, y)
		else 0 // Pixels outside the image are regarded as black
	}
	
	def computeGradient(color1: Int, color2: Int): Int = {
		((color1 - color2) / 2) + 127
	}
	
	def completed(): Unit = {
		for (x <- 0 until width; y <- 0 until height; l = luminosity(getPixel(x, y)))
			greyScale.setPixel(x, y, l)
		for (x <- 0 until width; y <- 0 until height; g = computeGradient(getGrey(x + 1, y), getGrey(x - 1, y)))
			xGradient.setPixel(x, y, g)
		for (x <- 0 until width; y <- 0 until height; g = computeGradient(getGrey(x, y + 1), getGrey(x, y - 1)))
			yGradient.setPixel(x, y, g)
	}
	
	private def luminosity(c: Color): Int = (0.2126 * c.getRed + 0.7152 * c.getGreen + 0.0722 * c.getBlue + 0.5).toInt
}