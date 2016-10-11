package org.edoardo

import java.awt.Color

object Main {
	val root = "/home/edoardo/Dropbox/UniversityWork/Project/bitmaps/disc/"
	val gt = Pixmap.load(root + "discGT.pbm").get
	val policy = new Policy[Decision, Neighbourhood]
	val numImages = 100
	val neighbourhoodSize = 3
	
	def reward(x: Int, y: Int, decision: Decision): Int = {
		val wasContained = gt.getPixel(x, y).equals(new Color(255, 255, 255))
		if (wasContained == decision.include) 1
		else -1
	}
	
	def main(args: Array[String]): Unit = {
		for (i <- 1 until numImages) {
			val img = Pixmap.load(root + "discMS" + i + ".pbm").get
			assert(img.width == gt.width && img.height == gt.height)
			val region = new Region(img.height, img.width)
			while (!region.completed()) {
				val (x, y) = region.getPixel
				val state = img.getNeighbourhood(x, y, neighbourhoodSize)
				val decision = policy.greedyPlay(state)
				policy.update(state, decision, reward(x, y, decision))
				if (decision.include)
					region.includePixel(x, y)
			}
			Pixmap.write(root + "discRes" + i + ".pbm", region.toBitmap)
		}
	}
}
