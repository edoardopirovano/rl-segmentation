package org.edoardo

import java.awt.Color

import Array.ofDim
import scala.collection.mutable

class Region(val height: Int, width: Int) {
	
	object Status extends Enumeration {
		val Selected, NotSelected, Pending, Filled, Unknown = Value
	}
	
	val status: Array[Array[Status.Value]] = ofDim[Status.Value](height, width)
	for (y <- 0 until height; x <- 0 until width)
		status(y)(x) = Status.Unknown
	
	def getStatus(xa: Int, ya: Int): Status.Value = {
		if (!contains(xa, ya))
			Status.NotSelected
		else if (status(ya)(xa) == Status.Pending)
			Status.Unknown
		else
			status(ya)(xa)
	}
	
	val toConsider: mutable.Queue[(Int, Int)] = mutable.Queue()
	
	def completed(): Boolean = toConsider.isEmpty
	
	def getPixel: (Int, Int) = toConsider.dequeue()
	
	def contains(x: Int, y: Int): Boolean = x >= 0 && y >= 0 && x < width && y < height
	
	def shouldConsider(x: Int, y: Int): Boolean = contains(x, y) && status(y)(x) == Status.Unknown
	
	def includePixel(x: Int, y: Int): Unit = {
		status(y)(x) = Status.Selected
		List((x - 1, y), (x + 1, y), (x, y - 1), (x, y + 1))
			.filter(p => shouldConsider(p._1, p._2))
			.foreach(p => doConsider(p._1, p._2))
	}
	
	def doConsider(x: Int, y: Int): Unit = {
		status(y)(x) = Status.Pending
		toConsider.enqueue((x, y))
	}
	
	def excludePixel(x: Int, y: Int): Unit = {
		status(y)(x) = Status.NotSelected
	}
	
	def didSelect(x: Int, y: Int): Boolean = status(y)(x) == Status.Selected
	
	// Dilate by one pixel to reduce noise
	def dilate(): Unit = {
		for (i <- 0 until height; j <- 0 until width) {
			if (getStatus(j, i) == Status.Selected) {
				if (i > 0 && status(i - 1)(j) != Status.Selected) status(i - 1)(j) = Status.Filled
				if (j > 0 && status(i)(j - 1) != Status.Selected) status(i)(j - 1) = Status.Filled
				if (i + 1 < width && status(i + 1)(j) != Status.Selected) status(i + 1)(j) = Status.Filled
				if (j + 1 < height && status(i)(j + 1) != Status.Selected) status(i)(j + 1) = Status.Filled
			}
		}
		for (i <- 0 until height; j <- 0 until width) {
			if (status(i)(j) == Status.Filled)
				status(i)(j) = Status.Selected
		}
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
