package org.edoardo.parser

import java.io.{BufferedInputStream, FileInputStream}

import org.edoardo.segmentation.SegmentationResult

/**
  * Implements a parser for a Multi Feature Set (MFS), which describes selections in an IPF. Note we make the simplifying
  * assumption that there is only one feature in the image, and it is called "Level Set 0"
  */
object MFS extends Parser {
	
	/**
	  * Load a MFS from a file.
	  * @param fileName the file to load the MFS from
	  * @param ipf the IPF corresponding to the MFS we are loading
	  * @return a segmentation result containing the region described by the MFS
	  */
	def loadFromFile(fileName: String, ipf: VolumeIPF): SegmentationResult = {
		implicit val in = new BufferedInputStream(new FileInputStream(fileName))
		checkLineIs("MFS Text 0")
		checkLineIs("{")
		checkLineIs("Level Set 0")
		checkLineIs("{")
		
		var done = false
		var regions: List[(Int, Int)] = List()
		while (!done) {
			val line: String = readLine
			if (line == "}")
				done = true
			else {
				val splitLine: Array[String] = line.split(",")
				regions ::= (splitLine(0).drop(1).toInt, splitLine(1).dropRight(1).toInt)
			}
		}
		
		checkLineIs("}")
		
		val result: Array[Array[Array[Boolean]]] = Array.fill[Boolean](ipf.width, ipf.height, ipf.depth)(false)
		for ((x, y, z) <- regions.flatMap(region => ipf.getRegionPixels(region._1, region._2)))
			result(x)(y)(z) = true
		new SegmentationResult(result)
	}
}
