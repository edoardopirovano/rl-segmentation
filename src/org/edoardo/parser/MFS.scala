package org.edoardo.parser

import java.io.{BufferedInputStream, FileInputStream}

import org.edoardo.segmentation.SegmentationResult

object MFS extends Parser {
	
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
