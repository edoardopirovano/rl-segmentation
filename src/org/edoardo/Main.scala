package org.edoardo

import java.awt.Color

object Main {
	val policy = new Policy[Decision, Neighbourhood]
	val numImages = 100
	val neighbourhoodSize = 3
	val includeFirst = 9
	
	def reward(x: Int, y: Int, decision: Decision, gt: RgbBitmap): Int = {
		val wasContained = gt.getPixel(x, y).equals(new Color(255, 255, 255))
		if (wasContained == decision.include) 1
		else -1
	}
	
	def analyseImage(img: RgbBitmap, gt: RgbBitmap) : Region = {
		if (gt != null)
			assert(img.width == gt.width && img.height == gt.height)
		val region = new Region(img.height, img.width)
		var i = 0
		while (!region.completed()) {
			i += 1
			val (x, y) = region.getPixel
			val state = img.getNeighbourhood(x, y, neighbourhoodSize, region)
			val decision = if (i <= includeFirst) Decision(true) else policy.greedyPlay(state)
			if (gt != null)
				policy.update(state, decision, reward(x, y, decision, gt))
			if (decision.include)
				region.includePixel(x, y)
			else
				region.excludePixel(x, y)
		}
		region
	}
	
	def doImage(name: String, gt: RgbBitmap) = {
		val img = Pixmap.load(name).get
		val region = analyseImage(img, gt)
		Pixmap.write(name.substring(0, name.length - 4) + "-result.pbm", region.toBitmap)
	}
	
	def main(args: Array[String]): Unit = {
		val root = "/home/edoardo/Dropbox/UniversityWork/Project/bitmaps/disc/"
		val discGT = Pixmap.load(root + "discGT.pbm").get
		for (i <- 1 until numImages)
			doImage(root + "discMS" + i + ".pbm", discGT)
		doImage(root + "fractal.pbm", null)
		doImage(root + "knee.pgm", null)
	}
}
