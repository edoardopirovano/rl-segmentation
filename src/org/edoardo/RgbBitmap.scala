package org.edoardo

import java.awt.image.BufferedImage
import java.awt.{Color, Graphics}

import scala.Array.ofDim

class RgbBitmap(val width: Int, val height: Int) {
	val image = new BufferedImage(width, height, BufferedImage.TYPE_3BYTE_BGR)
	
	val gradientMagnitude = new GreyscaleBitmap(width, height)
	val greyScale = new GreyscaleBitmap(width, height)
	
	def getState(x: Int, y: Int): PixelInfo = {
		PixelInfo(gradientMagnitude.getPixel(x, y), greyScale.getPixel(x, y))
	}
	
	def fill(c: Color): Unit = {
		val g: Graphics = image.getGraphics
		g.setColor(c)
		g.fillRect(0, 0, width, height)
	}
	
	def setPixel(x: Int, y: Int, c: Color): Unit = image.setRGB(x, y, c.getRGB)
	
	def completed(): Unit = {
		for (x <- 0 until width; y <- 0 until height; l = luminosity(getPixel(x, y)))
			greyScale.setPixel(x, y, l)
		for (x <- 0 until width; y <- 0 until height) {
			val xGradient: Int = computeGradient(getGrey(x + 1, y), getGrey(x - 1, y))
			val yGradient: Int = computeGradient(getGrey(x, y + 1), getGrey(x, y - 1))
			gradientMagnitude.setPixel(x, y, computeMagnitude(xGradient, yGradient))
		}
	}
	
	def getGrey(x: Int, y: Int): Int = {
		if (contains(x, y)) greyScale.getPixel(x, y)
		else 0 // Pixels outside the image are regarded as black
	}
	
	def contains(x: Int, y: Int): Boolean = x >= 0 && y >= 0 && x < width && y < height
	
	def computeGradient(color1: Int, color2: Int): Int = ((color1 - color2) / 2) + 127
	
	def computeMagnitude(xGradient: Int, yGradient: Int): Int = (Math.sqrt(xGradient ^ 2 + yGradient ^ 2).toInt / 360) * 255
	
	private def luminosity(c: Color): Int = (0.2126 * c.getRed + 0.7152 * c.getGreen + 0.0722 * c.getBlue + 0.5).toInt
	
	def toSegmentationResult: SegmentationResult = {
		val result: Array[Array[Boolean]] = ofDim[Boolean](height, width)
		for (x <- 0 until width; y <- 0 until height)
			result(y)(x) = getPixel(x, y).equals(new Color(255, 255, 255))
		new SegmentationResult(result)
	}
	
	def getPixel(x: Int, y: Int) = new Color(image.getRGB(x, y))
}