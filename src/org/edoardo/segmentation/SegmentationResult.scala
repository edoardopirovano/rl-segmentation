package org.edoardo.segmentation

import java.io.{BufferedOutputStream, FileOutputStream}

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
	
	def dilateAndErode(n: Int): Unit = {
		dilate(n)
		erode(n)
	}
	
	// Pre: manhattanDistanceToSelected is set
	def dilate(k: Int): Unit = {
		val manhattanDistanceToSelected: Array[Array[Int]] = computeManhattan(false)
		for (i <- 0 until height; j <- 0 until width; if manhattanDistanceToSelected(i)(j) <= k)
			selected(i)(j) = true
	}
	
	// Post: manhattanDistanceToSelected is not set
	def erode(k: Int): Unit = {
		val manhattanDistanceToNotSelected: Array[Array[Int]] = computeManhattan(true)
		for (i <- 0 until height; j <- 0 until width; if manhattanDistanceToNotSelected(i)(j) <= k)
			selected(i)(j) = false
	}
	
	def rewardChoosing(x: Int, y: Int): Int = rewardChoosingArray.get(y)(x)
	
	def rewardOmitting(x: Int, y: Int): Int = -rewardChoosingArray.get(y)(x)
	
	def writeTo(fileName: String): Unit = {
		val s = new BufferedOutputStream(new FileOutputStream(fileName))
		s.write(headerPbm(height, width).getBytes)
		for (y <- 0 until height) {
			for (x <- 0 until width) {
				if (doesContain(x, y)) s.write("0 ".getBytes)
				else s.write("1 ".getBytes)
			}
			s.write("\n".getBytes)
		}
		s.close()
	}
	
	def doesContain(x: Int, y: Int): Boolean = selected(y)(x)
	
	def headerPbm(height: Int, width: Int): String = "P1\n" + width + " " + height + "\n"
	
	def headerPgm(height: Int, width: Int): String = "P5\n" + width + " " + height + "\n255\n"
}
