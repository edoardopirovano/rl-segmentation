package org.edoardo.bitmap

import ij.plugin.filter.MaximumFinder
import ij.process.{ByteProcessor, ImageConverter, ImageProcessor}
import ij.{IJ, ImagePlus}
import net.imglib2.FinalInterval
import net.imglib2.`type`.numeric.integer.UnsignedByteType
import net.imglib2.algorithm.gradient.PartialDerivative
import net.imglib2.algorithm.pde.PeronaMalikAnisotropicDiffusion
import net.imglib2.img.Img
import net.imglib2.img.array.ArrayImgFactory
import net.imglib2.img.display.imagej.ImageJFunctions
import net.imglib2.util.Intervals
import net.imglib2.view.Views
import org.edoardo.segmentation.{RegionInfo, SegmentationResult}

import scala.Array.ofDim
import scala.collection.mutable

class WatershedRegion(parent: WrappedImage, var pixels: Set[(Int, Int)], var border: Set[(Int, Int)], var neighbours: Set[WatershedRegion]) {
	var cachedInfo: RegionInfo = _
	
	def getInfo: RegionInfo = {
		if (cachedInfo == null) {
			val avgIntensity: Int = pixels.map(p => parent.getPixel(p._1, p._2)).sum / pixels.size
			val maxBorderGradient: Int = border.map(p => parent.gradientImage.getPixel(p._1, p._2)(0)).max
			cachedInfo = RegionInfo(avgIntensity, maxBorderGradient)
		}
		cachedInfo
	}
}

class WrappedImage(val image: ImagePlus) {
	val debug = true
	val wrapped: Img[UnsignedByteType] = ImageJFunctions.wrapByte(image)
	
	new ImageConverter(image).convertToGray8()
	val width: Int = image.getWidth
	val height: Int = image.getHeight
	var gradientImage: ImagePlus = _
	var mapToRegion: mutable.Map[(Int, Int), WatershedRegion] = mutable.Map[(Int, Int), WatershedRegion]()

	def getPixel(x: Int, y: Int): Int = image.getPixel(x, y)(0)
	
	def doPreProcess(diffuse: Boolean, watershed: Boolean): Unit = {
		if (diffuse) anisotropicDiffuse(0.15, 20, 3)
		computeGradientImage()
		if (watershed) doWatershed()
	}
	
	def getRegion(x: Int, y: Int): Option[WatershedRegion] = mapToRegion.get((x, y))
	
	def contains(x: Int, y: Int): Boolean = x >= 0 && y >= 0 && x < width && y < height
	
	def toSegmentationResult: SegmentationResult = {
		val result: Array[Array[Boolean]] = ofDim[Boolean](height, width)
		for (x <- 0 until width; y <- 0 until height)
			result(y)(x) = getPixel(x, y).equals(255)
		new SegmentationResult(result)
	}
	
	private def anisotropicDiffuse(deltat: Double, kappa: Int, repeats: Int): Unit = {
		val filter = new PeronaMalikAnisotropicDiffusion(wrapped, deltat,
			new PeronaMalikAnisotropicDiffusion.WideRegionEnhancer(kappa))
		for (i <- 0 until repeats)
			filter.process()
		if (debug) image.show()
	}
	
	private def computeGradientImage(): Unit = {
		val gradients: Img[UnsignedByteType] = new ArrayImgFactory[UnsignedByteType]().create(Array(width, height, 3), new UnsignedByteType())
		val gradientComputationInterval: FinalInterval = Intervals.expand(wrapped, -1)
		for (d <- 0 until 2)
			PartialDerivative.gradientCentralDifference(wrapped, Views.interval(Views.hyperSlice(gradients, 2, d), gradientComputationInterval), d)
		gradientImage = ImageJFunctions.wrap(gradients, image.getTitle + "-gradient")
		if (debug) gradientImage.show()
	}
	
	private def doWatershed(): Unit = {
		val maximumFinder = new MaximumFinder()
		maximumFinder.setup(null, gradientImage)
		val watershed: ByteProcessor = maximumFinder.findMaxima(gradientImage.getProcessor, 0, ImageProcessor.NO_THRESHOLD, MaximumFinder.SEGMENTED, false, false)
		if (debug) {
			val watershedImage: ImagePlus = IJ.createImage(image.getTitle + "-watershed", width, height, 1, 8)
			watershedImage.setImage(watershed.createImage())
			watershedImage.show()
		}
		for (x <- 0 until watershed.getWidth; y <- 0 until watershed.getHeight) {
			if (watershed.getPixel(x, y) == 255 && mapToRegion.get((x, y)).isEmpty) {
				val region = new WatershedRegion(this, null, null, null)
				val pixels: mutable.Set[(Int, Int)] = mutable.Set[(Int, Int)]()
				val border: mutable.Set[(Int, Int)] = mutable.Set[(Int, Int)]()
				var toConsider: mutable.Set[(Int, Int)] = mutable.Set[(Int, Int)]((x, y))
				while (toConsider.nonEmpty) {
					val (a, b) = toConsider.head
					toConsider = toConsider.tail
					if (watershed.getPixel(a, b) == 255) {
						pixels.add((a, b))
						toConsider ++= getNeighbours(a, b).filter(p => contains(p._1, p._2) && !pixels.contains(p) && !border.contains(p))
						mapToRegion += ((a, b) -> region)
					} else border.add((a, b))
				}
				region.pixels = pixels.toSet
				region.border = border.toSet
			}
		}
		for (region <- mapToRegion.values) {
			val neighbours: mutable.Set[WatershedRegion] = mutable.Set[WatershedRegion]()
			for ((a, b) <- region.border)
				neighbours ++= getNeighbours(a, b)
					.map(p => getRegion(p._1, p._2))
					.filter(neighbour => neighbour.nonEmpty)
					.map(neighbour => neighbour.get)
			neighbours.remove(region)
			region.neighbours = neighbours.toSet
		}
	}
	
	private def getNeighbours(a: Int, b: Int) = List((a - 1, b), (a + 1, b), (a, b - 1), (a, b + 1))
	
}