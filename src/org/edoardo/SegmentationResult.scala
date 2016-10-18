package org.edoardo

import java.awt.Color
import java.io.{BufferedOutputStream, FileOutputStream}

class SegmentationResult(selected: Array[Array[Boolean]]) {
	val height: Int = selected.length
	val width: Int = selected(0).length
	
	def didSelect(x: Int, y: Int): Boolean = selected(y)(x)
	
	def dilate(): Unit = {
		var toSelect: List[(Int, Int)] = Nil
		for (i <- 0 until height; j <- 0 until width) {
			if (selected(i)(j)) {
				if (i > 0 && !selected(i - 1)(j)) toSelect ::= (i - 1, j)
				if (j > 0 && !selected(i)(j - 1)) toSelect ::= (i, j - 1)
				if (i + 1 < height && !selected(i + 1)(j)) toSelect ::= (i + 1, j)
				if (j + 1 < width && !selected(i)(j + 1)) toSelect ::= (i, j + 1)
			}
		}
		for ((x, y) <- toSelect)
			selected(x)(y) = true
	}
	
	def erode(): Unit = {
		var toDeselect: List[(Int, Int)] = Nil
		for (i <- 0 until height; j <- 0 until width) {
			if (!selected(i)(j)) {
				if (i > 0 && selected(i - 1)(j)) toDeselect ::= (i - 1, j)
				if (j > 0 && selected(i)(j - 1)) toDeselect ::= (i, j - 1)
				if (i + 1 < height && selected(i + 1)(j)) toDeselect ::= (i + 1, j)
				if (j + 1 < width && selected(i)(j + 1)) toDeselect ::= (i, j + 1)
			}
		}
		for ((x, y) <- toDeselect)
			selected(x)(y) = false
	}
	
	def dilateAndErode(n: Int): Unit = {
		for (i <- 0 until n)
			dilate()
		for (i <- 0 until n)
			erode()
	}
	
	def header(height: Int, width: Int): String = "P1\n" + width + " " + height + "\n"
	
	def writeTo(fileName: String): Unit = {
		val s = new BufferedOutputStream(new FileOutputStream(fileName))
		s.write(header(height, width).getBytes)
		for (y <- 0 until height) {
			for (x <- 0 until width) {
				if (didSelect(x, y)) s.write("0 ".getBytes)
				else s.write("1 ".getBytes)
			}
			s.write("\n".getBytes)
		}
		s.close()
	}
}
