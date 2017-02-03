package org.edoardo.segmentation

import ij.IJ
import org.edoardo.bitmap.{Raw, WrappedImage}
import org.edoardo.ipf.{IPF, VolumeIPF}
import org.edoardo.rl.Policy

object RLSegmentation {
	val epsilonReciprocal = 10
	val dilateErodeConstant = 3
	
	val policy = new Policy[Decision, RegionInfo]
	
	def main(args: Array[String]): Unit = {
		for (i <- List(1, 11))
			doImage("Knee" + i + "_0010.pgm", "knee" + i + ".ipf", "knee" + i + "result.tiff", (250, 180, 0), Some("Knee" + i + "_0010-gt.pgm"))
		doImage("Knee10_0010.pgm", "knee10.ipf", "knee10result.tiff", (250, 180, 0))
	}
	
	def doImage(name: String, ipfName: String, resultName: String, seed: (Int, Int, Int), gtName: Option[String] = None, numPracticeRuns: Int = 40): Unit = {
		val img: WrappedImage = new WrappedImage(if (name.takeRight(3) == "mhd") Raw.openImage(name) else IJ.openImage(name))
		val gt: Option[SegmentationResult] = gtName.map(name => new WrappedImage(IJ.openImage(name)).toSegmentationResult)
		img.doPreProcess()
		val ipf: VolumeIPF = IPF.loadFromFile(ipfName)
		if (gt.isDefined) {
			for (i <- 0 until numPracticeRuns) {
				val result: SegmentationResult = analyseImage(img, ipf, gt, seed)
				println(name.dropRight(4)  + "\t" + i + "\t" + score(result, gt.get))
				result.writeTo(resultName.dropRight(5) + "-" + i + ".tiff")
			}
		}
		analyseImage(img, ipf, None, seed).writeTo(resultName)
	}
	
	def getInfo(pixels: List[(Int, Int, Int)], img: WrappedImage): RegionInfo = {
		val avgIntensity: Int = pixels.map(p => img.getVoxel(p._1, p._2, p._3)).sum / pixels.size
		val maxGradient: Int = pixels.map(p => img.getGradient(p._1, p._2, p._3)).max
		RegionInfo(avgIntensity, maxGradient)
	}
	
	def analyseImage(img: WrappedImage, ipf: VolumeIPF, gt: Option[SegmentationResult], seed: (Int, Int, Int)): SegmentationResult = {
		if (gt.isDefined)
			assert(img.width == gt.get.width && img.height == gt.get.height)
		val selection = new Selection(img.height, img.width, img.depth, ipf)
		var decisions: List[(RegionInfo, Int, Decision)] = List()
		selection.startPixel(seed._1, seed._2, seed._3, img, if (gt.isDefined) 1 else 3)
		while (!selection.completed()) {
			val region: Int = selection.getRegion
			val state: RegionInfo = getInfo(ipf.getRegionPixels(region), img)
			val decision: Decision = if (gt.isEmpty) policy.greedyPlay(state) else policy.epsilonSoft(state, epsilonReciprocal)
			decisions ::= (state, region, decision)
			if (decision.include)
				selection.includeRegion(region)
			else
				selection.excludeRegion(region)
		}
		if (gt.isDefined)
			decisions.foreach { case (state, region, dec) => policy.update(state, dec, reward(region, dec.include, ipf, gt)) }
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
	
	def score(result: SegmentationResult, gt: SegmentationResult): Float = {
		var overlap = 0
		var resultSize = 0
		var gtSize = 0
		for (x <- 0 until result.width; y <- 0 until result.height; z <- 0 until result.depth) {
			if (result.doesContain(x, y, z) && gt.doesContain(x, y, z)) overlap += 1
			if (result.doesContain(x, y, z)) resultSize += 1
			if (gt.doesContain(x, y, z)) gtSize += 1
		}
		(2f * overlap) / (gtSize + resultSize)
	}
}