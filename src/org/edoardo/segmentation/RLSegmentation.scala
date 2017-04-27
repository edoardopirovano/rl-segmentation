package org.edoardo.segmentation

import java.io.File

import ij.IJ
import ij.io.{FileSaver, Opener}
import ij.plugin.FolderOpener
import org.edoardo.image.{Raw, WindowedImage}
import org.edoardo.parser.{IPF, MFS, VolumeIPF}
import org.edoardo.rl.Policy

import scala.collection.mutable

/**
  * Contains the main code for running the segmentation algorithm.
  */
object RLSegmentation {
	val policy = new Policy[Decision, RegionInfo]
	val opener = new Opener()
	val regionInfoCache: mutable.Map[Int, RegionInfo] = mutable.Map.empty
	val epsilonReciprocal = 10
	
	/**
	  * Main method to run our segmentation algorithm.
	  * @param args A number between one and three containing the number of the experiment to run.
	  */
	def main(args: Array[String]): Unit = {
		if (args(0) == "1")
			experiementOne()
		else if (args(0) == "2")
			experiementTwo()
		else if (args(0) == "3")
			experimentThree()
		else
			println("Invalid experiment number.")
	}
	
	/**
	  * The first experiment we ran on MRI scans from the Botnar Research Centre.
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
				"preTraining-" + imageInfo.id, imageInfo.seed, imageInfo.windowing,
				Some("knee" + imageInfo.id + "-layer" + imageInfo.layer + ".mfs"), imageInfo.layer, 0, saveAsRaw = false)
		
		println("-- Training --")
		for (imageInfo <- imageInfos.take(5))
			doImage(imageInfo.fileName, "knee" + imageInfo.id + "-layer" + imageInfo.layer + ".ipf",
				"training-" + imageInfo.id, imageInfo.seed, imageInfo.windowing,
				Some("knee" + imageInfo.id + "-layer" + imageInfo.layer + ".mfs"), imageInfo.layer, 40, saveAsRaw = false)
		
		// printPolicy()
		
		println("-- After Training --")
		for (imageInfo <- imageInfos)
			doImage(imageInfo.fileName, "knee" + imageInfo.id + "-layer" + imageInfo.layer + ".ipf",
				"postTraining-" + imageInfo.id, imageInfo.seed, imageInfo.windowing,
				Some("knee" + imageInfo.id + "-layer" + imageInfo.layer + ".mfs"), imageInfo.layer, 0, saveAsRaw = false)
	}
	
	/**
	  * The second experiment we ran on MRI scans from SKI10 grand challenge.
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
			ImageInfo(46, "image-046.mhd", 69, (100, 100, 0), (870, 1493)),
			ImageInfo(52, "image-052.mhd", 83, (100, 100, 0), (162, 321)),
			ImageInfo(54, "image-054.mhd", 75, (100, 100, 0), (598, 1013)),
			ImageInfo(60, "image-060.mhd", 76, (100, 100, 0), (415, 830))
		)
		println("-- Before Training --")
		for (imageInfo <- imageInfos)
			doImage(imageInfo.fileName, "image" + imageInfo.id + "-layer" + imageInfo.layer + ".ipf",
				"preTraining-" + imageInfo.id, imageInfo.seed, imageInfo.windowing,
				Some("labels-%03d.mhd".format(imageInfo.id.toInt)), imageInfo.layer, 0, saveAsRaw = false)
		
		println("-- Training --")
		for (imageInfo <- imageInfos.take(5))
			doImage(imageInfo.fileName, "image" + imageInfo.id + "-layer" + imageInfo.layer + ".ipf",
				"training-" + imageInfo.id, imageInfo.seed, imageInfo.windowing,
				Some("labels-%03d.mhd".format(imageInfo.id.toInt)), imageInfo.layer, 40, saveAsRaw = false)
		
		println("-- After Training --")
		for (imageInfo <- imageInfos)
			doImage(imageInfo.fileName, "image" + imageInfo.id + "-layer" + imageInfo.layer + ".ipf",
				"postTraining-" + imageInfo.id, imageInfo.seed, imageInfo.windowing,
				Some("labels-%03d.mhd".format(imageInfo.id.toInt)), imageInfo.layer, 0, saveAsRaw = false)
	}
	
	/**
	  * The third experiment we ran on XRay images.
	  */
	def experimentThree(): Unit = {
		val xrayInfos = List(
			XRayInfo(1, "knee1-gt.pgm", (264, 579, 0)),
			XRayInfo(3, "knee3-gt.pgm", (135, 237, 0)),
			XRayInfo(2, "knee2.mfs", (135, 250, 0))
		)
		println("-- Before Training --")
		for (xrayInfo <- xrayInfos)
			doImage("knee" + xrayInfo.id + ".pgm", "knee" + xrayInfo.id + ".ipf", "preTraining-" + xrayInfo.id,
				xrayInfo.seed, (127, 255), Some(xrayInfo.gt), 0, 0, saveAsRaw = false)
		
		println("-- Training --")
		for (xrayInfo <- xrayInfos.take(2))
			doImage("knee" + xrayInfo.id + ".pgm", "knee" + xrayInfo.id + ".ipf", "training-" + xrayInfo.id,
				xrayInfo.seed, (127, 255), Some(xrayInfo.gt), 0, 40, saveAsRaw = false)
		
		println("-- After Training --")
		for (xrayInfo <- xrayInfos)
			doImage("knee" + xrayInfo.id + ".pgm", "knee" + xrayInfo.id + ".ipf", "postTraining-" + xrayInfo.id,
				xrayInfo.seed, (127, 255), Some(xrayInfo.gt), 0, 0, saveAsRaw = false)
	}
	
	/**
	  * Apply our algorithm to an image.
	  *
	  * @param name            the name of the file (or folder) the image (or layers of the image) can be found in
	  * @param ipfName         the name of the file containing the IPF for the image
	  * @param resultName      the name of the result file to store the segmentation result in, should end in .tiff
	  * @param seed            the seed point to begin growing the region from
	  * @param windowing       the windowing to use, in the form of a pair of (centre, width)
	  * @param gtName          the name of the file containing the gold standard to compare with (and learn from, if applicable)
	  * @param stayInLayer     the layer to explore in (-1 to explore the whole image)
	  * @param numPracticeRuns the number of times to practice on this image (0 to not train on this image)
	  * @param saveAsRaw       whether to save the image as RAW (will be saved as TIFF otherwise)
	  */
	def doImage(name: String, ipfName: String, resultName: String, seed: (Int, Int, Int), windowing: (Int, Int) = (0, 0),
				gtName: Option[String] = None, stayInLayer: Integer = -1, numPracticeRuns: Int = 40, saveAsRaw: Boolean): Unit = {
		val img: WindowedImage = new WindowedImage(
			if (name.takeRight(3) == "mhd")
				Raw.openMetadata(name, stayInLayer + 1)
			else if (new File(name).isDirectory)
				new FolderOpener().openFolder(name)
			else IJ.openImage(name), windowing)
		new FileSaver(img.image).saveAsTiff(resultName + "-original.tiff")
		val ipf: VolumeIPF = IPF.loadFromFile(ipfName)
		val gt: Option[SegmentationResult] = gtName.map(name =>
			if (name.takeRight(3) == "mfs")
				MFS.loadFromFile(name, ipf)
			else
				new WindowedImage(
					if (name.takeRight(3) == "mhd")
						Raw.openLabels(name)
					else opener.openImage(name)
				).toSegmentationResult(stayInLayer))
		if (gt.isDefined)
			gt.get.writeTo(resultName + "-gt", saveAsRaw)
		img.doPreProcess()
		if (gt.isDefined) {
			for (i <- 0 until numPracticeRuns) {
				val result: SegmentationResult = analyseImage(img, ipf, gt, seed, stayInLayer != -1)
				println(name + "\t" + i + "\t" + score(result, gt.get))
				result.writeTo(resultName + "-" + i, saveAsRaw)
			}
		}
		val result: SegmentationResult = analyseImage(img, ipf, None, seed, stayInLayer != -1)
		if (gt.isDefined)
			println(name + "\tfin\t" + score(result, gt.get))
		result.writeTo(resultName, saveAsRaw)
		regionInfoCache.clear()
	}
	
	/**
	  * Get the information for a given region from the first branch layer of the IPF.
	  *
	  * @param region the identifier for the region
	  * @param ipf    the IPF of the image we are considering
	  * @param img    the image we are considering
	  * @return the information to be used by the agent to decide whether or not to include this region
	  */
	def getInfo(region: Int, ipf: VolumeIPF, img: WindowedImage): RegionInfo = {
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
	  *
	  * @param img         the image to analyse
	  * @param ipf         the IPF for the image
	  * @param gt          the gold standard to compare to, if applicable
	  * @param seed        the seed point to grow from
	  * @param stayInLayer whether or not to remain in the same layer
	  * @return the result of segmenting the image
	  */
	def analyseImage(img: WindowedImage, ipf: VolumeIPF, gt: Option[SegmentationResult], seed: (Int, Int, Int),
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
	  *
	  * @param region   the region considered
	  * @param decision whether or not the agent chose to include it
	  * @param ipf      the IPF for the
	  * @param gt       the gold standard we are comparing against (this function will return constant 0 if this is None)
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
	  *
	  * @param result the result of a segmentation
	  * @param gt     the gold standard we are comparing against
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
		(2f * overlap) / (gtSize + resultSize) + "\t" +
			overlap.toFloat / gtSize + "\t" +
			falsePositive.toFloat / (imageSize - gtSize)
	}
	
	/**
	  * Prints out the current policy (ie. what the agent believes to be the best action in every state).
	  */
	def printPolicy(): Unit = {
		println("-- Policy Learnt --")
		for (x <- 0 to 255) {
			for (y <- 0 to 255)
				print((if (!policy.haveEncountered(RegionInfo(List(x, y)))) 0
				else if (policy.greedyPlay(RegionInfo(List(x, y))).include) 1
				else -1) + "\t")
			println()
		}
		printPercentageSeen()
	}
	
	def printPercentageSeen(): Unit = {
		var total = 0
		var seen = 0
		for (x <- 0 to 255; y <- 0 to 255) {
			total += 1
			if (policy.haveEncountered(RegionInfo(List(x, y))))
				seen += 1
		}
		println("Percentage of states seen: " + 100 * (seen.toFloat / total))
	}
	
	private case class ImageInfo(id: Integer, fileName: String, layer: Integer, seed: (Int, Int, Int), windowing: (Int, Int))
	
	private case class XRayInfo(id: Integer, gt: String, seed: (Int, Int, Int))
	
}