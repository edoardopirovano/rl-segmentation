package org.edoardo

object Main {
	val practiceRepeats = 100
	val epsilonReciprocal = 10
	val includeFirst = 9
	
	val policy = new Policy[Decision, PixelInfo]
	
	def main(args: Array[String]): Unit = {
		doImage("knee.pgm", "knee1result.pbm", Some("knee-gt.pgm"), (230, 150))
		doImage("knee2.pgm", "knee2result.pbm", Some("knee2-gt.pgm"), (150, 100))
		doImage("knee.pgm", "knee1resultFinal.pbm", None, (242, 142))
		doImage("knee2.pgm", "knee2resultFinal.pbm", None, (175, 135))
		doImage("knee3.pgm", "knee3resultFinal.pbm", None, (250, 180))
		doImage("knee4.pgm", "knee4resultFinal.pbm", None, (130, 117))
	}
	
	def doImage(name: String, resultName: String, gtName: Option[String], seed: (Int, Int)): Unit = {
		val gt: Option[SegmentationResult] = gtName.map(name => Bitmap.load(name).get.toSegmentationResult)
		gt.foreach(result => result.completeGT())
		val img: RgbBitmap = Bitmap.load(name).get
		if (gt.isDefined) {
			for (i <- 0 until practiceRepeats)
				analyseImage(img, gt, seed).writeTo(resultName.substring(0, resultName.length - 4) + "-" + i + ".pbm")
		}
		analyseImage(img, None, seed).writeTo(resultName)
	}
	
	def analyseImage(img: RgbBitmap, gt: Option[SegmentationResult], seed: (Int, Int)): SegmentationResult = {
		if (gt.isDefined)
			assert(img.width == gt.get.width && img.height == gt.get.height)
		val region = new Region(img.height, img.width)
		region.doConsider(seed._1, seed._2)
		var i = 0
		var decisions: List[(PixelInfo, Decision)] = Nil
		while (!region.completed()) {
			i += 1
			val (x, y) = region.getPixel
			val state: PixelInfo = img.getState(x, y)
			val decision: Decision = if (i <= includeFirst) Decision(true) else if (gt.isEmpty) policy.greedyPlay(state) else policy.epsilonSoft(state, epsilonReciprocal)
			decisions ::= (state, decision)
			if (decision.include)
				region.includePixel(x, y)
			else
				region.excludePixel(x, y)
		}
		val result: SegmentationResult = region.getResult
		result.dilateAndErode(3)
		if (gt.isDefined) {
			var totalReward = 0
			for (x <- 0 until img.width; y <- 0 until img.height; included = result.doesContain(x, y))
				totalReward += reward(x, y, included, gt)
			for ((state, decision, reward) <- decisions)
				policy.update(state, decision, totalReward)
		}
		result
	}
	
	/**
	  * This gives a positive reward for everything inside the GT and a negative one for things just
	  * outside it. It gives no reward for things unrelated to the GT since we don't care about
	  * these (we only want to avoid leaving the target area).
	  */
	def reward(x: Int, y: Int, decision: Boolean, gt: Option[SegmentationResult]): Int = {
		if (gt.isEmpty) return 0
		if (decision) gt.get.rewardChoosing(x, y)
		else gt.get.rewardOmitting(x, y)
	}
}