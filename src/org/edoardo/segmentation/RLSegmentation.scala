package org.edoardo.segmentation

import ij.IJ
import org.edoardo.bitmap.WrappedImage
import org.edoardo.ipf.{IPF, VolumeIPF}
import org.edoardo.rl.Policy

object RLSegmentation {
	val epsilonReciprocal = 10
	val dilateErodeConstant = 3
	
	val policy = new Policy[Decision, RegionInfo]
	
	def main(args: Array[String]): Unit = {
		for (i <- List(1, 11))
			doImage("Knee" + i + "_0010.pgm", "knee" + i + ".ipf", "knee" + i + "result.pgm", (250, 180), Some("Knee" + i + "_0010-gt.pgm"))
		doImage("Knee10_0010.pgm", "knee10.ipf", "knee10result.pgm", (250, 180))
	}
	
	def doImage(name: String, ipfName: String, resultName: String, seed: (Int, Int), gtName: Option[String] = None, numPracticeRuns: Int = 40): Unit = {
		val gt: Option[SegmentationResult] = gtName.map(name => new WrappedImage(IJ.openImage(name)).toSegmentationResult)
		gt.foreach(result => result.completeGT())
		val img: WrappedImage = new WrappedImage(IJ.openImage(name))
		img.doPreProcess()
		val ipf: VolumeIPF = IPF.loadFromFile(ipfName)
		if (gt.isDefined) {
			for (i <- 0 until numPracticeRuns)
				analyseImage(img, ipf, gt, seed).writeTo(resultName.substring(0, resultName.length - 4) + "-" + i + ".pgm")
		}
		analyseImage(img, ipf, None, seed).writeTo(resultName)
	}
	
	def getInfo(pixels: List[(Int, Int)], img: WrappedImage): RegionInfo = {
		val avgIntensity: Int = pixels.map(p => img.getPixel(p._1, p._2)).sum / pixels.size
		val maxGradient: Int = pixels.map(p => img.gradientImage.getPixel(p._1, p._2)(0)).max
		RegionInfo(avgIntensity, maxGradient)
	}
	
	def analyseImage(img: WrappedImage, ipf: VolumeIPF, gt: Option[SegmentationResult], seed: (Int, Int)): SegmentationResult = {
		if (gt.isDefined)
			assert(img.width == gt.get.width && img.height == gt.get.height)
		val selection = new Selection(img.height, img.width, ipf)
		var decisions: List[(RegionInfo, Int, Decision)] = List()
		selection.startPixel(seed._1, seed._2, img, if (gt.isDefined) 1 else 3)
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
		val result: SegmentationResult = selection.getResult
		if (gt.isDefined)
			decisions.foreach { case (state, region, dec) => policy.update(state, dec, reward(region, dec.include, ipf, gt)) }
		result
	}
	
	def reward(region: Int, decision: Boolean, ipf: VolumeIPF, gt: Option[SegmentationResult]): Int = {
		if (gt.isEmpty) return 0
		val pixels: List[(Int, Int)] = ipf.getRegionPixels(region)
		var reward: Int = 0
		for ((x, y) <- pixels)
			reward += gt.get.rewardChoosing(x, y)
		if (reward == -pixels.size) return 0 // Ignore decisions on regions entirely outside goal
		if (decision) reward
		else -reward
	}
}