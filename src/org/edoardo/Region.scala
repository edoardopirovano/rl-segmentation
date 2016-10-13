package org.edoardo

import java.awt.Color

import Array.ofDim
import scala.collection.mutable

class Region(val height: Int, width: Int) {
	val status = ofDim[Status.Value](height, width)
	for (y <- 0 until height; x <- 0 until width)
		status(y)(x) = Status.Unknown
	status(height/2)(width/2) = Status.Pending
	
	def getStatus(xa: Int, ya: Int) = {
		if (!contains(xa, ya))
			Status.NotSelected
		else if (status(ya)(xa) == Status.Pending)
			Status.Unknown
		else
			status(ya)(xa)
	}
	
	val toConsider = mutable.Queue((height/2, width/2))
	
	def completed(): Boolean = toConsider.isEmpty
	
	def getPixel = toConsider.dequeue()
	
	def contains(x: Int, y: Int): Boolean = x >= 0 && y >= 0 && x < width && y < height
	
	def shouldConsider(x: Int, y: Int): Boolean = contains(x, y) && status(y)(x) == Status.Unknown
	
	def includePixel(x: Int, y: Int) = {
		status(y)(x) = Status.Selected
		List((x - 1, y), (x + 1, y), (x, y - 1), (x, y + 1))
		    .filter(p => shouldConsider(p._1, p._2))
			.foreach(p => {
				status(p._2)(p._1) = Status.Pending
				toConsider.enqueue(p)
			})
	}
	
	def excludePixel(x: Int, y: Int) = {
		status(y)(x) = Status.NotSelected
	}
	
	def toBitmap: RgbBitmap = {
		val bm = new RgbBitmap(width, height)
		for (y <- 0 until height; x <- 0 until width) {
			if (status(y)(x) == Status.Selected)
				bm.setPixel(x, y, new Color(255, 255, 255))
			else
				bm.setPixel(x, y, new Color(0, 0, 0))
		}
		bm
	}
}
