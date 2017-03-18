package org.edoardo.image

import java.io.{BufferedInputStream, File, FileInputStream}

import com.google.common.io.LittleEndianDataInputStream
import ij.process.ImageProcessor
import ij.{IJ, ImagePlus}

/**
  * Provides a few methods for working with Raw image files.
  */
object Raw {
	sealed trait ByteType
	case object UCHAR extends ByteType
	case object USHORT extends ByteType
	
	/**
	  * Read the image described by the given metadata file.
	  * @param name the name of the metadata file for a Raw image
	  * @param layer the layer to read (or -1 to read all layers)
	  * @return the image read
	  */
	def openMetadata(name: String, layer: Integer): ImagePlus = {
		val (width, height, depth, dataFile, byteType) = readMetadata(name)
		assert(byteType == USHORT)
		implicit val in = new LittleEndianDataInputStream(new BufferedInputStream(new FileInputStream(dataFile)))
		val image: ImagePlus = IJ.createImage(name, "16-bit", width, height, if (layer == -1) depth else 1)
		for (z <- 0 until depth) {
				for (y <- 0 until height) {
					for (x <- 0 until width) {
						val intensity: Short = in.readShort()
						if (layer == -1 || z == layer)
							image.getProcessor.putPixel(x, y, intensity)
					}
				}
			if (layer == -1)
				image.setZ(image.getZ + 1)
		}
		image
	}
	
	/**
	  * Open a label image (contains 1 for marked voxels, 0 for unmarked ones).
	  * @param name the name of the label image to open
	  * @return an image containing white for marked voxels and black for unmarked ones
	  */
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
	
	/**
	  * Read metadata from a metadata file.
	  * @param name the name of the metadata file
	  * @return a tuple containing, in order, the width of the image, the height of the image, the depth of the image,
	  *         the name of the file containing the image data, the type of the each voxel in the image data
	  */
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
	
	/**
	  * Read a short from an input stream.
	  * @param in the stream to read from
	  * @return the short read
	  */
	def readShort(implicit in: LittleEndianDataInputStream): Int = in.readShort
}
