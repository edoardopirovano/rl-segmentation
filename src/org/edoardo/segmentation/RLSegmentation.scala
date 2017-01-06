package org.edoardo.segmentation

import ij.IJ
import org.edoardo.bitmap.{WatershedRegion, WrappedImage}
import org.edoardo.rl.Policy

object RLSegmentation {
	val epsilonReciprocal = 10
	val dilateErodeConstant = 3
	
	val policy = new Policy[Decision, RegionInfo]
	
	def main(args: Array[String]): Unit = {
		doImage("knee1.pgm", "knee1result.pbm", (230, 150), Some("knee1-gt.pgm"))
		doImage("knee2.pgm", "knee2result.pbm", (150, 100), Some("knee2-gt.pgm"))
		doImage("knee3.pgm", "knee3result.pbm", (250, 180), Some("knee3-gt.pgm"))
		doImage("knee4.pgm", "knee4result.pbm", (125, 125), Some("knee4-gt.pgm"))
		doImage("knee10.pgm", "knee10result.pbm", (250, 180))
	}
	
	def doImage(name: String, resultName: String, seed: (Int, Int), gtName: Option[String] = None, numPracticeRuns: Int = 40): Unit = {
		val gt: Option[SegmentationResult] = gtName.map(name => new WrappedImage(IJ.openImage(name)).toSegmentationResult)
		gt.foreach(result => result.completeGT())
		val img: WrappedImage = new WrappedImage(IJ.openImage(name))
		img.doPreProcess(diffuse = true, watershed = true)
		if (gt.isDefined) {
			for (i <- 0 until numPracticeRuns)
				analyseImage(img, gt, seed).writeTo(resultName.substring(0, resultName.length - 4) + "-" + i + ".pbm")
		}
		analyseImage(img, None, seed).writeTo(resultName)
	}
	
	def analyseImage(img: WrappedImage, gt: Option[SegmentationResult], seed: (Int, Int)): SegmentationResult = {
		if (gt.isDefined)
			assert(img.width == gt.get.width && img.height == gt.get.height)
		val selection = new Selection(img.height, img.width)
		var decisions: List[(RegionInfo, WatershedRegion, Decision)] = List()
		selection.startPixel(seed._1, seed._2, img)
		while (!selection.completed()) {
			val region: WatershedRegion = selection.getRegion
			val state: RegionInfo = region.getInfo
			val decision: Decision = if (gt.isEmpty) policy.greedyPlay(state) else policy.epsilonSoft(state, epsilonReciprocal)
			decisions ::= (state, region, decision)
			if (decision.include)
				selection.includeRegion(region)
			else
				selection.excludeRegion(region)
		}
		val result: SegmentationResult = selection.getResult
		if (gt.isDefined)
			decisions.foreach { case (state, region, dec) => policy.update(state, dec, reward(region, dec.include, gt)) }
		result.dilateAndErode(dilateErodeConstant)
		result
	}
	
	def reward(region: WatershedRegion, decision: Boolean, gt: Option[SegmentationResult]): Int = {
		if (gt.isEmpty) return 0
		var reward: Int = 0
		for ((x, y) <- region.pixels)
			reward += gt.get.rewardChoosing(x, y)
		if (reward == -region.pixels.size) return 0 // Ignore decisions on regions entierly outside goal
		if (decision) reward
		else -reward
	}
}