package org.edoardo.bitmap

import ij.ImagePlus
import ij.process.{ImageConverter, ImageProcessor}
import net.imglib2.FinalInterval
import net.imglib2.`type`.numeric.integer.UnsignedByteType
import net.imglib2.algorithm.gradient.PartialDerivative
import net.imglib2.img.Img
import net.imglib2.img.array.ArrayImgFactory
import net.imglib2.img.display.imagej.ImageJFunctions
import net.imglib2.util.Intervals
import net.imglib2.view.Views
import org.edoardo.segmentation.SegmentationResult

import scala.Array.ofDim

class WrappedImage(val image: ImagePlus) {
	def getGradient(x: Int, y: Int, z: Int): Int = {
		(0 until dimensions).map(dir => gradientProcessors(dir)(z).getPixel(x, y)).max
	}
	
	val debug = true
	val wrapped: Img[UnsignedByteType] = ImageJFunctions.wrapByte(image)
	
	new ImageConverter(image).convertToGray8()
	val width: Int = image.getWidth
	val height: Int = image.getHeight
	val depth: Int = image.getDimensions()(3)
	val dimensions: Int = if (depth == 1) 2 else 3
	val processors: Array[ImageProcessor] = (0 until depth).map(z => image.getImageStack.getProcessor(z + 1)).toArray
	var gradientProcessors: Array[Array[ImageProcessor]] = _
	
	def getVoxel(x: Int, y: Int, z: Int): Int = processors(z).getPixel(x, y)
	
	def doPreProcess(): Unit = {
		computeGradientImage()
	}
	
	def contains(x: Int, y: Int): Boolean = x >= 0 && y >= 0 && x < width && y < height
	
	def toSegmentationResult: SegmentationResult = {
		val result: Array[Array[Array[Boolean]]] = ofDim[Boolean](width, height, depth)
		for (x <- 0 until width; y <- 0 until height; z <- 0 until depth)
			result(x)(y)(z) = getVoxel(x, y, z).equals(255)
		new SegmentationResult(result)
	}
	
	private def computeGradientImage(): Unit = {
		val gradients: Img[UnsignedByteType] = new ArrayImgFactory[UnsignedByteType]().create(if (dimensions == 2) Array(width, height, 2) else Array(width, height, depth, dimensions), new UnsignedByteType())
		val gradientComputationInterval: FinalInterval = Intervals.expand(wrapped, -1)
		for (d <- 0 until dimensions)
			PartialDerivative.gradientCentralDifference(wrapped, Views.interval(Views.hyperSlice(gradients, dimensions, d), gradientComputationInterval), d)
		val gradientImage: ImagePlus = ImageJFunctions.wrap(gradients, image.getShortTitle + "-gradients")
		gradientProcessors = (0 until dimensions).map(dir => {
			(0 until depth).map(z => gradientImage.getImageStack.getProcessor((dir * depth) + z + 1)).toArray
		}).toArray
		if (debug) gradientImage.show()
	}
}