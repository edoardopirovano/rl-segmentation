package org.edoardo.bitmap

import java.io.{BufferedInputStream, FileInputStream}

import com.google.common.io.LittleEndianDataInputStream
import ij.{IJ, ImagePlus}

object Raw {
	def openImage(name: String): ImagePlus = {
		val (width, height, depth, dataFile) = readMetadata(name)
		implicit val in = new LittleEndianDataInputStream(new BufferedInputStream(new FileInputStream(dataFile)))
		val unwindowed: ImagePlus = IJ.createImage(name, "16-bit", width, height, depth)
		var minIntensity = Integer.MAX_VALUE
		var maxIntensity = Integer.MIN_VALUE
		for (z <- 0 until depth) {
			for (y <- 0 until height) {
				for (x <- 0 until width) {
					val intensity: Short = in.readShort()
					minIntensity = Math.min(intensity, minIntensity)
					maxIntensity = Math.max(intensity, maxIntensity)
					unwindowed.getProcessor.putPixel(x, y, intensity)
				}
			}
			unwindowed.setZ(unwindowed.getZ + 1)
		}
		val windowed: ImagePlus = IJ.createImage(name, "8-bit", width, height, depth)
		for (z <- 1 to depth) {
			windowed.setZ(z)
			unwindowed.setZ(z)
			for (x <- 0 until width; y <- 0 until height)
				windowed.getProcessor.putPixel(x, y, Math.round(255 * ((unwindowed.getPixel(x, y)(0) - minIntensity).toFloat / (maxIntensity - minIntensity))))
		}
		windowed
	}
	
	def openLabels(name: String): ImagePlus = {
		val (width, height, depth, dataFile) = readMetadata(name)
		implicit val in = new LittleEndianDataInputStream(new BufferedInputStream(new FileInputStream(dataFile)))
		val labels: ImagePlus = IJ.createImage(name, "8-bit", width, height, depth)
		for (z <- 0 until depth) {
			for (y <- 0 until height) {
				for (x <- 0 until width) {
					labels.getProcessor.putPixel(x, y, in.readByte())
				}
			}
		}
		labels
	}
	
	def readMetadata(name: String): (Int, Int, Int, String) = {
		var width = 0
		var height = 0
		var depth = 0
		var dataFile = ""
		for (line <- scala.io.Source.fromFile(name).getLines().map(line => line.split(" = "))) {
			if (line(0) == "DimSize") {
				val dims: Array[String] = line(1).split(" ")
				width = dims(0).toInt
				height = dims(1).toInt
				depth = dims(2).toInt
			}
			if (line(0) == "ElementDataFile")
				dataFile = line(1)
		}
		(width, height, depth, dataFile)
	}
	
	def readShort(implicit in: LittleEndianDataInputStream): Int = in.readShort
}
