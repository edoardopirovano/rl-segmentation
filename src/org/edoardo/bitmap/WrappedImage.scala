package org.edoardo.bitmap

import ij.ImagePlus
import ij.process.ImageConverter
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
	val debug = true
	val wrapped: Img[UnsignedByteType] = ImageJFunctions.wrapByte(image)
	
	new ImageConverter(image).convertToGray8()
	val width: Int = image.getWidth
	val height: Int = image.getHeight
	var gradientImage: ImagePlus = _

	def getPixel(x: Int, y: Int): Int = image.getPixel(x, y)(0)
	
	def doPreProcess(): Unit = {
		computeGradientImage()
	}
	
	def contains(x: Int, y: Int): Boolean = x >= 0 && y >= 0 && x < width && y < height
	
	def toSegmentationResult: SegmentationResult = {
		val result: Array[Array[Boolean]] = ofDim[Boolean](height, width)
		for (x <- 0 until width; y <- 0 until height)
			result(y)(x) = getPixel(x, y).equals(255)
		new SegmentationResult(result)
	}
	
	private def computeGradientImage(): Unit = {
		val gradients: Img[UnsignedByteType] = new ArrayImgFactory[UnsignedByteType]().create(Array(width, height, 3), new UnsignedByteType())
		val gradientComputationInterval: FinalInterval = Intervals.expand(wrapped, -1)
		for (d <- 0 until 2)
			PartialDerivative.gradientCentralDifference(wrapped, Views.interval(Views.hyperSlice(gradients, 2, d), gradientComputationInterval), d)
		gradientImage = ImageJFunctions.wrap(gradients, image.getTitle + "-gradient")
		if (debug) gradientImage.show()
	}
	
}