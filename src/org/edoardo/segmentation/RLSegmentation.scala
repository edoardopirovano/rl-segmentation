package org.edoardo.segmentation

import java.io.File

import ij.IJ
import ij.io.{FileSaver, Opener}
import ij.plugin.FolderOpener
import org.edoardo.bitmap.{Raw, WrappedImage}
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
	
	def doImage(name: String, ipfName: String, resultName: String, seed: (Int, Int, Int), windowing: (Int, Int) = (0, 0),
				gtName: Option[String] = None, stayInLayer: Integer = -1, numPracticeRuns: Int = 40): Unit = {
		val img: WrappedImage = new WrappedImage(
			if (name.takeRight(3) == "mhd")
				Raw.openMetadata(name)
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
	
	def analyseImage(img: WrappedImage, ipf: VolumeIPF, gt: Option[SegmentationResult], seed: (Int, Int, Int),
					 stayInLayer: Boolean): SegmentationResult = {
		if (gt.isDefined)
			assert(img.width == gt.get.width && img.height == gt.get.height && img.depth == gt.get.depth)
		val selection = new Selection(img.height, img.width, img.depth, ipf, stayInLayer)
		var decisions: List[(RegionInfo, Int, Decision)] = List()
		selection.startPixel(seed._1, seed._2, seed._3, img, if (gt.isDefined) 2 else 3)
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
	
	def reward(region: Int, decision: Boolean, ipf: VolumeIPF, gt: Option[SegmentationResult]): Int = {
		if (gt.isEmpty) return 0
		val pixels: List[(Int, Int, Int)] = ipf.getRegionPixels(region)
		var reward: Int = 0
		for ((x, y, z) <- pixels)
			reward += (if (gt.get.doesContain(x, y, z)) 1 else -1)
		if (decision) reward
		else -reward
	}
	
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