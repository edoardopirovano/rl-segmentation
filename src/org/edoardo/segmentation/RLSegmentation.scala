package org.edoardo.segmentation

import java.io.File

import ij.IJ
import ij.io.{FileSaver, Opener}
import ij.plugin.FolderOpener
import org.edoardo.image.{Raw, WrappedImage}
import org.edoardo.parser.{IPF, MFS, VolumeIPF}
import org.edoardo.rl.Policy

import scala.collection.mutable

object RLSegmentation {
	val policy = new Policy[Decision, RegionInfo]
	val opener = new Opener()
	val regionInfoCache: mutable.Map[Int, RegionInfo] = mutable.Map.empty
	val epsilonReciprocal = 10
	
	private case class ImageInfo(id: Integer, fileName: String, layer: Integer, seed: (Int, Int, Int), windowing: (Int, Int))
	
	def main(args: Array[String]): Unit = {
		if (args(0) == "one")
			experiementOne()
		else if (args(0) == "two")
			experiementTwo()
		else
			println("Invalid experiment number.")
	}
	
	/**
	  * The first experiment we ran on CT scans from the Botnar Research Centre.
	  */
	def experiementOne(): Unit = {
		val imageInfos = List(
			// Training data
			ImageInfo(1, "Knee1/Knee1_0009.dcm", 10, (200, 200, 0), (1214, 2408)),
			ImageInfo(10, "Knee10/Knee10_0009.dcm", 10, (200, 200, 0), (1050, 2050)),
			ImageInfo(11, "Knee11/Knee11_0009.dcm", 9, (200, 200, 0), (1100, 2200)),
			ImageInfo(12, "Knee12/Knee12_0009.dcm", 10, (200, 200, 0), (1050, 2050)),
			ImageInfo(13, "Knee13/Knee13_0010.dcm", 10, (200, 200, 0), (1050, 2050)),
			
			// Evaluation data
			ImageInfo(2, "Knee2/Knee2_0006.dcm", 6, (200, 150, 0), (1325, 1604)),
			ImageInfo(3, "Knee3/Knee3_0010.dcm", 11, (200, 200, 0), (1126, 2231)),
			ImageInfo(5, "Knee5/Knee5_0008.dcm", 9, (200, 200, 0), (1275, 2500)),
			ImageInfo(6, "Knee6/Knee6_0010.dcm", 11, (200, 200, 0), (1200, 2400)),
			ImageInfo(7, "Knee7/Knee7_0010.dcm", 11, (200, 150, 0), (1050, 2050))
		)
		println("-- Before Training --")
		for (imageInfo <- imageInfos)
			doImage(imageInfo.fileName, "knee" + imageInfo.id + "-layer" + imageInfo.layer + ".ipf",
				"preTraining-" + imageInfo.id + ".tiff", imageInfo.seed, imageInfo.windowing,
				Some("knee" + imageInfo.id + "-layer" + imageInfo.layer + ".mfs"), imageInfo.layer, 0)
		
		println("-- Training --")
		for (imageInfo <- imageInfos.take(5))
			doImage(imageInfo.fileName, "knee" + imageInfo.id + "-layer" + imageInfo.layer + ".ipf",
				"training-" + imageInfo.id + ".tiff", imageInfo.seed, imageInfo.windowing,
				Some("knee" + imageInfo.id + "-layer" + imageInfo.layer + ".mfs"), imageInfo.layer, 40)
		
		println("-- After Training --")
		for (imageInfo <- imageInfos)
			doImage(imageInfo.fileName, "knee" + imageInfo.id + "-layer" + imageInfo.layer + ".ipf",
				"postTraining-" + imageInfo.id + ".tiff", imageInfo.seed, imageInfo.windowing,
				Some("knee" + imageInfo.id + "-layer" + imageInfo.layer + ".mfs"), imageInfo.layer, 0)
	}
	
	/**
	  * The second experiment we can on MRI scans from SKI10 grand challenge.
	  */
	def experiementTwo(): Unit = {
		val imageInfos = List(
			// Training data
			ImageInfo(4, "image-004.mhd", 66, (100, 100, 0), (416, 781)),
			ImageInfo(15, "image-015.mhd", 70, (150, 100, 0), (251, 492)),
			ImageInfo(41, "image-041.mhd", 59, (100, 140, 0), (79, 150)),
			ImageInfo(42, "image-042.mhd", 61, (100, 100, 0), (492, 985)),
			ImageInfo(50, "image-050.mhd", 64, (100, 100, 0), (64, 128)),
			
			// Evaluation data
			ImageInfo(39, "image-039.mhd", 69, (100, 100, 0), (64, 127)),
			ImageInfo(46, "image-046.mhd", 69, (100, 100, 0), (870,1493)),
			ImageInfo(52, "image-052.mhd", 83, (100, 100, 0), (162, 321)),
			ImageInfo(54, "image-054.mhd", 75, (100, 100, 0), (598, 1013)),
			ImageInfo(60, "image-060.mhd", 76, (100, 100, 0), (415, 830))
		)
		println("-- Before Training --")
		for (imageInfo <- imageInfos)
			doImage(imageInfo.fileName, "image" + imageInfo.id + "-layer" + imageInfo.layer + ".ipf",
				"preTraining-" + imageInfo.id + ".tiff", imageInfo.seed, imageInfo.windowing,
				Some("image" + imageInfo.id + "-layer" + imageInfo.layer + ".mfs"), imageInfo.layer, 0)
		
		println("-- Training --")
		for (imageInfo <- imageInfos.take(5))
			doImage(imageInfo.fileName, "image" + imageInfo.id + "-layer" + imageInfo.layer + ".ipf",
				"training-" + imageInfo.id + ".tiff", imageInfo.seed, imageInfo.windowing,
				Some("image" + imageInfo.id + "-layer" + imageInfo.layer + ".mfs"), imageInfo.layer, 40)
		
		println("-- After Training --")
		for (imageInfo <- imageInfos)
			doImage(imageInfo.fileName, "image" + imageInfo.id + "-layer" + imageInfo.layer + ".ipf",
				"postTraining-" + imageInfo.id + ".tiff", imageInfo.seed, imageInfo.windowing,
				Some("image" + imageInfo.id + "-layer" + imageInfo.layer + ".mfs"), imageInfo.layer, 0)
	}
	
	/**
	  * Apply our algorithm to an image.
	  * @param name the name of the file (or folder) the image (or layers of the image) can be found in
	  * @param ipfName the name of the file containing the IPF for the image
	  * @param resultName the name of the result file to store the segmentation result in, should end in .tiff
	  * @param seed the seed point to begin growing the region from
	  * @param windowing the windowing to use, in the form of a pair of (centre, width)
	  * @param gtName the name of the file containing the gold standard to compare with (and learn from, if applicable)
	  * @param stayInLayer the layer to explore in (-1 to explore the whole image)
	  * @param numPracticeRuns the number of times to practice on this image (0 to not train on this image)
	  */
	def doImage(name: String, ipfName: String, resultName: String, seed: (Int, Int, Int), windowing: (Int, Int) = (0, 0),
				gtName: Option[String] = None, stayInLayer: Integer = -1, numPracticeRuns: Int = 40): Unit = {
		val img: WrappedImage = new WrappedImage(
			if (name.takeRight(3) == "mhd")
				Raw.openMetadata(name, stayInLayer + 1)
			else if (new File(name).isDirectory)
				new FolderOpener().openFolder(name)
			else IJ.openImage(name), windowing)
		new FileSaver(img.image).saveAsTiff(resultName.dropRight(5) + "-original.tiff")
		val ipf: VolumeIPF = IPF.loadFromFile(ipfName)
		val gt: Option[SegmentationResult] = gtName.map(name =>
				if (name.takeRight(3) == "mfs")
					MFS.loadFromFile(name, ipf)
				else
					new WrappedImage(
						if (name.takeRight(3) == "mhd")
							Raw.openLabels(name)
						else opener.openImage(name)
					).toSegmentationResult(stayInLayer))
		if (gt.isDefined)
			gt.get.writeTo(resultName.dropRight(5) + "-gt.tiff")
		img.doPreProcess()
		if (gt.isDefined) {
			for (i <- 0 until numPracticeRuns) {
				val result: SegmentationResult = analyseImage(img, ipf, gt, seed, stayInLayer != -1)
				println(name + "\t" + i + "\t" + score(result, gt.get))
				result.writeTo(resultName.dropRight(5) + "-" + i + ".tiff")
			}
		}
		val result: SegmentationResult = analyseImage(img, ipf, None, seed, stayInLayer != -1)
		if (gt.isDefined)
			println(name + "\tfin\t" + score(result, gt.get))
		result.writeTo(resultName)
		regionInfoCache.clear()
	}
	
	/**
	  * Get the information for a given region from the first branch layer of the IPF.
	  * @param region the identifier for the region
	  * @param ipf the IPF of the image we are considering
	  * @param img the image we are considering
	  * @return the information to be used by the agent to decide whether or not to include this region
	  */
	def getInfo(region: Int, ipf: VolumeIPF, img: WrappedImage): RegionInfo = {
		regionInfoCache.getOrElseUpdate(region, {
			val pixels: List[(Int, Int, Int)] = ipf.getRegionPixels(region)
			val avgIntensity: Int = pixels.map(p => img.getVoxel(p._1, p._2, p._3)).sum / pixels.size
			// val minIntensity: Int = pixels.map(p => img.getVoxel(p._1, p._2, p._3)).min
			// val maxIntensity: Int = pixels.map(p => img.getVoxel(p._1, p._2, p._3)).max
			// val avgGradient: Int = pixels.map(p => img.getGradient(p._1, p._2, p._3)).sum / pixels.size
			val maxGradient: Int = pixels.map(p => img.getGradient(p._1, p._2, p._3)).max
			RegionInfo(List(avgIntensity, maxGradient))
		})
	}
	
	/**
	  * Analyse the given image.
	  * @param img the image to analyse
	  * @param ipf the IPF for the image
	  * @param gt the gold standard to compare to, if applicable
	  * @param seed the seed point to grow from
	  * @param stayInLayer whether or not to remain in the same layer
	  * @return the result of segmenting the image
	  */
	def analyseImage(img: WrappedImage, ipf: VolumeIPF, gt: Option[SegmentationResult], seed: (Int, Int, Int),
					 stayInLayer: Boolean): SegmentationResult = {
		if (gt.isDefined)
			assert(img.width == gt.get.width && img.height == gt.get.height && img.depth == gt.get.depth)
		val selection = new Selection(img.height, img.width, img.depth, ipf, stayInLayer)
		var decisions: List[(RegionInfo, Int, Decision)] = List()
		selection.startPixel(seed._1, seed._2, seed._3, if (gt.isDefined) 2 else 3)
		while (!selection.completed()) {
			val region: Int = selection.getRegion
			val state: RegionInfo = getInfo(region, ipf, img)
			val decision: Decision =
				if (gt.isEmpty) policy.greedyPlay(state)
				else policy.epsilonSoft(state, epsilonReciprocal)
			decisions ::= (state, region, decision)
			if (decision.include)
				selection.includeRegion(region)
			else
				selection.excludeRegion(region)
		}
		if (gt.isDefined)
			decisions.foreach {
				case (state, region, dec) =>
					policy.update(state, dec, reward(region, dec.include, ipf, gt))
			}
		val result: SegmentationResult = selection.getResult
		result.closeResult()
		result
	}
	
	/**
	  * Calculates the reward to give our agent for deciding to include or exclude a given region.
	  * @param region the region considered
	  * @param decision whether or not the agent chose to include it
	  * @param ipf the IPF for the
	  * @param gt the gold standard we are comparing against (this function will return constant 0 if this is None)
	  * @return a value corresponding to how many more pixels the decision was correct for (so, this will be a positive
	  *         value if the correct decision was made, and negative otherwise)
	  */
	def reward(region: Int, decision: Boolean, ipf: VolumeIPF, gt: Option[SegmentationResult]): Int = {
		if (gt.isEmpty) return 0
		val pixels: List[(Int, Int, Int)] = ipf.getRegionPixels(region)
		var reward: Int = 0
		for ((x, y, z) <- pixels)
			reward += (if (gt.get.doesContain(x, y, z)) 1 else -1)
		if (decision) reward
		else -reward
	}
	
	/**
	  * Create a string containing the scores of a segmentation compared to a gold standard.
	  * @param result the result of a segmentation
	  * @param gt the gold standard we are comparing against
	  * @return A tab separated String of values consisting of the DSC, TPVF and FPVF of the segmentation.
	  */
	def score(result: SegmentationResult, gt: SegmentationResult): String = {
		var overlap = 0
		var resultSize = 0
		var gtSize = 0
		var falsePositive = 0
		val imageSize: Int = gt.height * gt.width * gt.depth
		for (x <- 0 until result.width; y <- 0 until result.height; z <- 0 until result.depth) {
			if (result.doesContain(x, y, z) && gt.doesContain(x, y, z)) overlap += 1
			if (result.doesContain(x, y, z)) resultSize += 1
			if (gt.doesContain(x, y, z)) gtSize += 1
			if (result.doesContain(x, y, z) && !gt.doesContain(x, y, z)) falsePositive += 1
		}
		(2f * overlap) / (gtSize + resultSize) + "\t"  +
			overlap.toFloat / gtSize + "\t" +
			falsePositive.toFloat / (imageSize - gtSize)
	}
}