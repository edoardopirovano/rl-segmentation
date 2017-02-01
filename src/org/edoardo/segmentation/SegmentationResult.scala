package org.edoardo.segmentation

import ij.io.FileSaver
import ij.{IJ, ImagePlus}
import inra.ijpb.morphology.GeodesicReconstruction

import scala.Array.ofDim

class SegmentationResult(selected: Array[Array[Boolean]]) {
	val height: Int = selected.length
	val width: Int = selected(0).length
	
	var rewardChoosingArray: Option[Array[Array[Int]]] = None
	
	def completeGT(): Unit = {
		val manhattanDistanceToTarget: Array[Array[Int]] = computeManhattan(false)
		rewardChoosingArray = Some(ofDim[Int](height, width))
		for (i <- 0 until height; j <- 0 until width) {
			if (manhattanDistanceToTarget(i)(j) == 0)
				rewardChoosingArray.get(i)(j) = 1
			else
				rewardChoosingArray.get(i)(j) = -1
		}
	}
	
	def rewardChoosing(x: Int, y: Int): Int = rewardChoosingArray.get(y)(x)
	
	def rewardOmitting(x: Int, y: Int): Int = -rewardChoosingArray.get(y)(x)
	
	def closeResult(): Unit = {
		val result: ImagePlus = buildResult()
		val closedResult: ImagePlus = IJ.createImage("closedResult", "8-bit", width, height, 1)
		closedResult.setProcessor(GeodesicReconstruction.fillHoles(result.getProcessor))
		for (y <- 0 until height; x <- 0 until width)
			selected(y)(x) = closedResult.getPixel(x, y)(0) == 255
	}
	
	def buildResult(): ImagePlus = {
		val result: ImagePlus = IJ.createImage("result", "8-bit", width, height, 1)
		for (y <- 0 until height; x <- 0 until width) {
			if (doesContain(x, y))
				result.getProcessor.putPixel(x, y, 255)
			else
				result.getProcessor.putPixel(x, y, 0)
		}
		result
	}
	
	def writeTo(fileName: String): Unit = {
		new FileSaver(buildResult()).saveAsPgm(fileName)
	}
	
	def doesContain(x: Int, y: Int): Boolean = selected(y)(x)
	
	private def computeManhattan(isConverse: Boolean): Array[Array[Int]] = {
		val manhattan: Array[Array[Int]] = ofDim[Int](height, width)
		for (i <- 0 until height; j <- 0 until width) {
			if ((selected(i)(j) && !isConverse) || (!selected(i)(j) && isConverse)) {
				manhattan(i)(j) = 0
			} else {
				manhattan(i)(j) = width + height
				if (i > 0) manhattan(i)(j) = Math.min(manhattan(i)(j), manhattan(i - 1)(j) + 1)
				if (j > 0) manhattan(i)(j) = Math.min(manhattan(i)(j), manhattan(i)(j - 1) + 1)
			}
		}
		for (i <- height - 1 to 0 by -1; j <- width - 1 to 0 by -1) {
			if (i + 1 < height) manhattan(i)(j) = Math.min(manhattan(i)(j), manhattan(i + 1)(j) + 1)
			if (j + 1 < width) manhattan(i)(j) = Math.min(manhattan(i)(j), manhattan(i)(j + 1) + 1)
		}
		manhattan
	}
}
