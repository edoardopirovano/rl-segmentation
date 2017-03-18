package org.edoardo.bitmap

import java.io.{BufferedInputStream, File, FileInputStream}

import com.google.common.io.LittleEndianDataInputStream
import ij.process.ImageProcessor
import ij.{IJ, ImagePlus}

object Raw {
	sealed trait ByteType
	case object UCHAR extends ByteType
	case object USHORT extends ByteType
	
	def openMetadata(name: String, stayInLayer: Integer): ImagePlus = {
		val (width, height, depth, dataFile, byteType) = readMetadata(name)
		assert(byteType == USHORT)
		implicit val in = new LittleEndianDataInputStream(new BufferedInputStream(new FileInputStream(dataFile)))
		val image: ImagePlus = IJ.createImage(name, "16-bit", width, height, if (stayInLayer == -1) depth else 1)
		for (z <- 0 until depth) {
				for (y <- 0 until height) {
					for (x <- 0 until width) {
						val intensity: Short = in.readShort()
						if (stayInLayer == -1 || z == stayInLayer)
							image.getProcessor.putPixel(x, y, intensity)
					}
				}
			if (stayInLayer == -1)
				image.setZ(image.getZ + 1)
		}
		image
	}
	
	def openLabels(name: String): ImagePlus = {
		val (width, height, depth, dataFile, byteType) = readMetadata(name)
		implicit val in = new LittleEndianDataInputStream(new BufferedInputStream(new FileInputStream(
			if (new File(name).getParent == null) dataFile else new File(name).getParent + "/" + dataFile)))
		val labels: ImagePlus = IJ.createImage(name, "8-bit", width, height, depth)
		for (z <- 1 to depth) {
			labels.setZ(z)
			val processor: ImageProcessor = labels.getProcessor
			for (y <- 0 until height) {
				for (x <- 0 until width) {
					val value: Short = if (byteType == USHORT) in.readShort else in.readByte
					processor.putPixel(x, y, if (value == 1) 255 else 0)
				}
			}
		}
		labels
	}
	
	def readMetadata(name: String): (Int, Int, Int, String, ByteType) = {
		var width = 0
		var height = 0
		var depth = 0
		var dataFile = ""
		var byteType: ByteType = USHORT
		for (line <- scala.io.Source.fromFile(name).getLines().map(line => line.split(" = "))) {
			if (line(0) == "DimSize") {
				val dims: Array[String] = line(1).split(" ")
				width = dims(0).toInt
				height = dims(1).toInt
				depth = dims(2).toInt
			}
			if (line(0) == "ElementDataFile")
				dataFile = line(1)
			if (line(0) == "ElementType")
				byteType = line(1) match {
					case "MET_UCHAR" => UCHAR
					case "MET_USHORT" => USHORT
					case "MET_SHORT" => USHORT
					case _ => throw new IllegalArgumentException("Byte type of MHD file unsupported: " + line(1))
				}
		}
		(width, height, depth, dataFile, byteType)
	}
	
	def readShort(implicit in: LittleEndianDataInputStream): Int = in.readShort
}
