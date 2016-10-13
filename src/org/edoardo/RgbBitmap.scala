package org.edoardo

import java.awt.image.BufferedImage
import java.awt.Color

import scala.collection.mutable.ListBuffer

class RgbBitmap(val width: Int, val height: Int) {
	val image = new BufferedImage(width, height, BufferedImage.TYPE_3BYTE_BGR)
	
	def fill(c: Color) = {
		val g = image.getGraphics
		g.setColor(c)
		g.fillRect(0, 0, width, height)
	}
	
	def setPixel(x: Int, y: Int, c: Color) = image.setRGB(x, y, c.getRGB)
	
	def getPixel(x: Int, y: Int) = new Color(image.getRGB(x, y))
	
	def contains(x: Int, y: Int): Boolean = x >= 0 && y >= 0 && x < width && y < height
	
	def getPixelOrBlack(x: Int, y: Int) = {
		if (contains(x, y)) getPixel(x, y)
		else new Color(0, 0, 0)
	}
	
	def getNeighbourhood(x: Int, y: Int, n: Int, region: Region): Neighbourhood = {
		val margin: Int = n / 2
		val contents = new ListBuffer[List[Cell]]
		for (ya <- y - margin to y + margin)
			contents += (x - margin to x + margin).map(xa => Cell(getPixelOrBlack(xa, ya), region.getStatus(xa, ya))).toList
		Neighbourhood(contents.toList)
	}
}

object BitmapOps {
	def luminosity(c: Color) = (0.2126 * c.getRed + 0.7152 * c.getGreen + 0.0722 * c.getBlue + 0.5).toInt
	
	def greyscale(bm: RgbBitmap) = {
		val image = new RgbBitmap(bm.width, bm.height)
		for (x <- 0 until bm.width; y <- 0 until bm.height; l = luminosity(bm.getPixel(x, y)))
			image.setPixel(x, y, new Color(l, l, l))
		image
	}
}
