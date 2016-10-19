package org.edoardo.segmentation

import scala.Array.ofDim
import scala.collection.mutable

class Region(val height: Int, width: Int) {
	
	val status: Array[Array[Status.Value]] = ofDim[Status.Value](height, width)
	val toConsider: mutable.Queue[(Int, Int)] = mutable.Queue()
	for (y <- 0 until height; x <- 0 until width)
		status(y)(x) = Status.NotSelected
	
	def completed(): Boolean = toConsider.isEmpty
	
	def getPixel: (Int, Int) = toConsider.dequeue()
	
	def doesContain(x: Int, y: Int): Boolean = status(y)(x) == Status.Selected
	
	def includePixel(x: Int, y: Int): Unit = {
		status(y)(x) = Status.Selected
		List((x - 1, y), (x + 1, y), (x, y - 1), (x, y + 1))
			.filter(p => shouldConsider(p._1, p._2))
			.foreach(p => doConsider(p._1, p._2))
	}
	
	def shouldConsider(x: Int, y: Int): Boolean = contains(x, y) && status(y)(x) == Status.NotSelected
	
	def contains(x: Int, y: Int): Boolean = x >= 0 && y >= 0 && x < width && y < height
	
	def doConsider(x: Int, y: Int): Unit = {
		status(y)(x) = Status.Pending
		toConsider.enqueue((x, y))
	}
	
	def excludePixel(x: Int, y: Int): Unit = {
		status(y)(x) = Status.NotSelected
	}
	
	def getResult: SegmentationResult = new SegmentationResult(status.map(row => row.map(_ == Status.Selected)))
	
	object Status extends Enumeration {
		val Selected, NotSelected, Pending = Value
	}
	
}
