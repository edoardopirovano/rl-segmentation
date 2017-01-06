package org.edoardo.segmentation

import org.edoardo.bitmap.{WatershedRegion, WrappedImage}

import scala.collection.mutable

class Selection(val height: Int, width: Int) {
	
	var toConsider: mutable.Set[WatershedRegion] = mutable.Set[WatershedRegion]()
	val excluded: mutable.Set[WatershedRegion] = mutable.Set[WatershedRegion]()
	val included: mutable.Set[WatershedRegion] = mutable.Set[WatershedRegion]()
	
	def completed(): Boolean = toConsider.isEmpty
	
	def getRegion: WatershedRegion = {
		val result: WatershedRegion = toConsider.head
		toConsider = toConsider.tail
		result
	}
	
	def includeRegion(region: WatershedRegion): Unit = {
		included += region
		for (neighbour <- region.neighbours) {
			if (!excluded.contains(neighbour) && !included.contains(neighbour))
				toConsider += neighbour
		}
	}
	
	def startPixel(x: Int, y: Int, img: WrappedImage): Unit = {
		val neighborhood: List[(Int, Int)] = neighbours(x, y)
		for ((a, b) <- neighborhood) {
			if (img.getRegion(a, b).nonEmpty)
				includeRegion(img.getRegion(a, b).get)
		}
	}
	
	private def neighbours(x: Int, y:Int) = List((x - 1, y), (x + 1, y), (x, y - 1), (x, y + 1),
		(x + 1, y + 1), (x + 1, y - 1), (x - 1, y + 1), (x - 1, y - 1))
	
	def excludeRegion(region: WatershedRegion): Unit = {
		excluded += region
	}
	
	def getResult: SegmentationResult = {
		val status: Array[Array[Boolean]] = Array.fill(height) { Array.fill(width) { false } }
		for ((x, y) <- included.flatMap(region => region.pixels ++ region.border))
			status(y)(x) = true
		new SegmentationResult(status)
	}
	
}
