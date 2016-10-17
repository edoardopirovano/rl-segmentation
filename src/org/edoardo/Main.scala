package org.edoardo

import java.awt.Color

object Main {
	val practiceRepeats = 5
	val epsilonReciprocal = 10
	val includeFirst = 9
	
	val policy = new Policy[Decision, PixelInfo]
	
	def reward(x: Int, y: Int, decision: Decision, gt: RgbBitmap): Int = {
		val wasContained: Boolean = gt.getPixel(x, y).equals(new Color(255, 255, 255))
		if (wasContained == decision.include) 1
		else -1
	}
	
	def analyseImage(img: RgbBitmap, gt: Option[RgbBitmap], seed: (Int, Int)): Region = {
		if (gt.isDefined)
			assert(img.width == gt.get.width && img.height == gt.get.height)
		val region = new Region(img.height, img.width)
		region.doConsider(seed._1, seed._2)
		var i = 0
		while (!region.completed()) {
			i += 1
			val (x, y) = region.getPixel
			val state: PixelInfo = img.getState(x, y)
			val decision: Decision = if (i <= includeFirst) Decision(true) else if (gt.isEmpty) policy.greedyPlay(state) else policy.epsilonSoft(state, epsilonReciprocal)
			if (decision.include)
				region.includePixel(x, y)
			else
				region.excludePixel(x, y)
		}
		region.dilate()
		if (gt.isDefined) {
			for (x <- 0 until img.width; y <- 0 until img.height)
				policy.update(img.getState(x, y), Decision(region.didSelect(x, y)),
					reward(x, y, Decision(region.didSelect(x, y)), gt.get))
		}
		region
	}
	
	def doImage(name: String, resultName: String, gtName: Option[String], seed: (Int, Int)): Unit = {
		val gt: Option[RgbBitmap] = gtName.map(name => Bitmap.load(name).get)
		val img: RgbBitmap = Bitmap.load(name).get
		if (gt.isDefined) {
			for (i <- 0 until practiceRepeats)
				Bitmap.write(resultName.substring(0, resultName.length - 4) + "-" + i + ".pbm", analyseImage(img, gt, seed).toBitmap)
		}
		Bitmap.write(resultName, analyseImage(img, gt, seed).toBitmap)
	}
	
	def main(args: Array[String]): Unit = {
		doImage("knee.pgm", "knee1result.pbm", Some("knee-gt.pgm"), (230, 150))
		doImage("knee2.pgm", "knee2result.pbm", Some("knee2-gt.pgm"), (150, 100))
		doImage("knee.pgm", "knee1resultFinal.pbm", None, (242, 142))
		doImage("knee2.pgm", "knee2resultFinal.pbm", None, (175, 135))
		doImage("knee3.pgm", "knee3resultFinal.pbm", None, (250, 180))
	}
}