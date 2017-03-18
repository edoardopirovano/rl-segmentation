package org.edoardo.segmentation

import ij.io.FileSaver
import ij.process.ImageProcessor
import ij.{IJ, ImagePlus}
import inra.ijpb.morphology.{GeodesicReconstruction, GeodesicReconstruction3D}

/**
  * Stores the result of a segmentation and provides a few methods for working with it.
  * @param selected an array containing which voxels were selected
  */
class SegmentationResult(selected: Array[Array[Array[Boolean]]]) {
	val width: Int = selected.length
	val height: Int = selected(0).length
	val depth: Int = selected(0)(0).length
	
	/**
	  * Morphologically close the result stored by this object.
	  */
	def closeResult(): Unit = {
		val result: ImagePlus = buildResult()
		val closedResult: ImagePlus = IJ.createImage("closedResult", "8-bit", width, height, depth)
		if (depth > 1)
			closedResult.setStack(GeodesicReconstruction3D.fillHoles(result.getImageStack))
		else
			closedResult.setProcessor(GeodesicReconstruction.fillHoles(result.getProcessor))
		for (z <- 0 until depth) {
			closedResult.setZ(z + 1)
			for (y <- 0 until height; x <- 0 until width)
				selected(x)(y)(z) = closedResult.getPixel(x, y)(0) == 255
		}
	}
	
	/**
	  * Build up an image of the result stored by this object.
	  * @return an image corresponding to the result, with selected pixels white and the rest black
	  */
	def buildResult(): ImagePlus = {
		val result: ImagePlus = IJ.createImage("result", "8-bit", width, height, depth)
		for (z <- 1 to depth) {
			result.setZ(z)
			val processor: ImageProcessor = result.getProcessor
			for (y <- 0 until height; x <- 0 until width) {
				if (doesContain(x, y, z - 1))
					processor.putPixel(x, y, 255)
				else
					processor.putPixel(x, y, 0)
			}
		}
		result
	}
	
	/**
	  * Build a result image and write it out to a file, overwriting it if it already exists.
	  * @param fileName the name of the file to write the result image to (should end in .tiff)
	  */
	def writeTo(fileName: String): Unit = {
		new FileSaver(buildResult()).saveAsTiff(fileName)
	}
	
	/**
	  * Check if a given voxel is in the selection.
	  * @param x the x coordinate
	  * @param y the y coordinate
	  * @param z the z coordinate
	  * @return whether or not the given voxel is in the selection
	  */
	def doesContain(x: Int, y: Int, z: Int): Boolean = selected(x)(y)(z)
}
