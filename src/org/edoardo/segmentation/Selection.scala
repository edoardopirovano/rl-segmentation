package org.edoardo.segmentation

import org.edoardo.bitmap.WrappedImage
import org.edoardo.ipf.VolumeIPF

import scala.collection.mutable

class Selection(val height: Int, width: Int, depth: Int, ipf: VolumeIPF) {
	var toConsider: mutable.Set[Int] = mutable.Set[Int]()
	val excluded: mutable.Set[Int] = mutable.Set[Int]()
	val included: mutable.Set[Int] = mutable.Set[Int]()
	
	def completed(): Boolean = toConsider.isEmpty
	
	def getRegion: Int = {
		val result: Int = toConsider.head
		toConsider = toConsider.tail
		result
	}
	
	def includeRegion(region: Int): Unit = {
		included += region
		for (neighbour <- ipf.getNeighbours(region)) {
			if (!excluded.contains(neighbour) && !included.contains(neighbour))
				toConsider += neighbour
		}
	}
	
	def startPixel(x: Int, y: Int, z: Int, img: WrappedImage, layer: Int): Unit = {
		val startRegions: List[Int] = ipf.getRegionsInLayer(layer, x, y, z)
		for (region <- startRegions)
			includeRegion(region)
	}
	
	def excludeRegion(region: Int): Unit = {
		excluded += region
	}
	
	def getResult: SegmentationResult = {
		val status: Array[Array[Array[Boolean]]] = Array.ofDim(width, height, depth)
		for ((x, y, z) <- included.flatMap(region => ipf.getRegionPixels(region)))
			status(x)(y)(z) = true
		new SegmentationResult(status)
	}
	
}
