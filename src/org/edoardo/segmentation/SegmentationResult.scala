package org.edoardo.segmentation

import ij.io.FileSaver
import ij.{IJ, ImagePlus}
import inra.ijpb.morphology.{GeodesicReconstruction, GeodesicReconstruction3D}

class SegmentationResult(selected: Array[Array[Array[Boolean]]]) {
	val height: Int = selected.length
	val width: Int = selected(0).length
	val depth: Int = selected(0)(0).length
	
	var rewardChoosingArray: Option[Array[Array[Int]]] = None
	
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
	
	def buildResult(): ImagePlus = {
		val result: ImagePlus = IJ.createImage("result", "8-bit", width, height, depth)
		for (z <- 0 until depth) {
			result.setZ(z)
			for (y <- 0 until height; x <- 0 until width) {
				if (doesContain(x, y, z))
					result.getProcessor.putPixel(x, y, 255)
				else
					result.getProcessor.putPixel(x, y, 0)
			}
		}
		result
	}
	
	def writeTo(fileName: String): Unit = {
		new FileSaver(buildResult()).saveAsTiff(fileName)
	}
	
	def doesContain(x: Int, y: Int, z: Int): Boolean = selected(x)(y)(z)
}
