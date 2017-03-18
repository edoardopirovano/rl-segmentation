package org.edoardo.segmentation

import org.edoardo.parser.VolumeIPF

import scala.collection.mutable

/**
  * Keeps track of a growing selection and provides methods to include or exclude regions and find out which regions (if
  * any) still need to be considered for inclusion.
  * @param height the height of the image this selection is in
  * @param width the width of the image this selection is in
  * @param depth the depth of the image this selection is in
  * @param ipf the IPF we are using for the image
  * @param stayInLayer whether or not we should remain in the layer we start in
  */
class Selection(val height: Int, width: Int, depth: Int, ipf: VolumeIPF, stayInLayer: Boolean) {
	val toConsider: mutable.Set[Int] = mutable.Set[Int]()
	var toConsiderQueue: mutable.Queue[Int] = mutable.Queue[Int]()
	val excluded: mutable.Set[Int] = mutable.Set[Int]()
	val included: mutable.Set[Int] = mutable.Set[Int]()
	var firstZ: Int = -1
	
	/**
	  * Check if we have finished considering regions.
	  * @return whether or not we have finished considering regions
	  */
	def completed(): Boolean = toConsider.isEmpty
	
	/**
	  * Get a region that we still need to consider. Note this should only be called if completed() is false.
	  * @return a region that still needs considering
	  */
	def getRegion: Int = {
		val result: Int = toConsiderQueue.dequeue()
		toConsider.remove(result)
		result
	}
	
	/**
	  * Add a region to our selection, and add its neighbours to the regions we need to consider (if they have not
	  * already been included or excluded).
	  * @param region the region to add to our selection
	  */
	def includeRegion(region: Int): Unit = {
		included += region
		for (neighbour <- ipf.getNeighbours(region)) {
			if (!excluded.contains(neighbour) && !included.contains(neighbour)) {
				if (!(stayInLayer && ipf.getZ(region) != firstZ) && toConsider.add(neighbour))
					toConsiderQueue.enqueue(neighbour)
			}
		}
	}
	
	/**
	  * Seed our selection with a starting region corresponding to the region at a given coordinate in a given layer
	  * of the IPF.
	  * @param x the x coordinate of our seed
	  * @param y the y coordinate of our seed
	  * @param z the z coordinate of our seed
	  * @param layer the layer of the IPF to start our selection in
	  */
	def startPixel(x: Int, y: Int, z: Int, layer: Int): Unit = {
		val startRegions: List[Int] = ipf.getRegionsInLayer(layer, x, y, z)
		firstZ = z
		for (region <- startRegions)
			includeRegion(region)
	}
	
	/**
	  * Exclude a region from our selection, preventing it from being considered again. This is necessary to avoid
	  * infinitely looping if there is a cycle of regions we do not wish to include.
	  * @param region the region to exclude
	  */
	def excludeRegion(region: Int): Unit = {
		excluded += region
	}
	
	/**
	  * Get the result of the segmentation corresponding to this selection.
	  * @return the result of the segmentation corresponding to this selection
	  */
	def getResult: SegmentationResult = {
		val status: Array[Array[Array[Boolean]]] = Array.ofDim(width, height, depth)
		for ((x, y, z) <- included.flatMap(region => ipf.getRegionPixels(region)))
			status(x)(y)(z) = true
		new SegmentationResult(status)
	}
	
}
