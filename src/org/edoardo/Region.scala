package org.edoardo

import java.awt.Color

import Array.ofDim
import scala.collection.mutable

class Region(val height: Int, width: Int) {
	val status = ofDim[Int](height, width)
	for (y <- 0 until height; x <- 0 until width)
		status(y)(x) = 0
	status(height/2)(width/2) = -1
	
	val toConsider = mutable.Queue((height/2, width/2))
	
	def completed(): Boolean = toConsider.isEmpty
	
	def getPixel = toConsider.dequeue()
	
	def contains(x: Int, y: Int): Boolean = x >= 0 && y >= 0 && x < width && y < height
	
	def shouldConsider(x: Int, y: Int): Boolean = contains(x, y) && status(y)(x) == 0
	
	def includePixel(x: Int, y: Int) = {
		status(y)(x) = 1
		List((x - 1, y), (x + 1, y), (x, y - 1), (x, y + 1))
		    .filter(p => shouldConsider(p._1, p._2))
			.foreach(p => {
				status(p._2)(p._1) = -1
				toConsider.enqueue(p)
			})
	}
	
	def toBitmap: RgbBitmap = {
		val bm = new RgbBitmap(width, height)
		for (y <- 0 until height; x <- 0 until width) {
			if (status(y)(x) == 1)
				bm.setPixel(x, y, new Color(255, 255, 255))
			else
				bm.setPixel(x, y, new Color(0, 0, 0))
		}
		bm
	}
}
