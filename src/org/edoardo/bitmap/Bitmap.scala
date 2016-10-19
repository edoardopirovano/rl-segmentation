package org.edoardo.bitmap

import java.awt.Color
import java.io.{BufferedInputStream, FileInputStream, InputStream}

object Bitmap {
	
	def load(filename: String): Option[RgbBitmap] = {
		implicit val in = new BufferedInputStream(new FileInputStream(filename))
		
		val header: PbmHeader = readHeader
		
		if (header.format == "P1") {
			// Black and white ASCII
			val bm = new RgbBitmap(header.width, header.height)
			
			for (y <- 0 until bm.height) {
				val line: String = readLine
				val parts: Array[String] = line.split("\\s")
				for (x <- 0 until bm.width) {
					if (parts(x).equals("1"))
						bm.setPixel(x, y, new Color(0, 0, 0))
					else
						bm.setPixel(x, y, new Color(255, 255, 255))
				}
			}
			bm.completed()
			Some(bm)
		} else if (header.format == "P2") {
			// Greyscale ASCII
			val bm = new RgbBitmap(header.width, header.height)
			val range: String = readLine
			assert(range.toInt == 255)
			
			for (y <- 0 until bm.height) {
				val line: String = readLine
				val parts: Array[String] = line.split("\\s")
				for (x <- 0 until bm.width) {
					val color: Int = parts(x).toInt
					bm.setPixel(x, y, new Color(color, color, color))
				}
			}
			bm.completed()
			Some(bm)
		} else if (header.format == "P5") {
			// Greyscale binary
			val bm = new RgbBitmap(header.width, header.height)
			val range: String = readLine
			assert(range.toInt == 255)
			
			for (y <- 0 until bm.height; x <- 0 until bm.width) {
				val color: Int = in.read
				bm.setPixel(x, y, new Color(color, color, color))
			}
			bm.completed()
			Some(bm)
		} else None
	}
	
	private def readHeader(implicit in: InputStream): PbmHeader = {
		val format: String = readLine
		var line: String = readLine
		
		while (line.startsWith("#"))
			line = readLine
		
		val parts: Array[String] = line.split("\\s")
		val width: Int = parts(0).toInt
		val height: Int = parts(1).toInt
		
		PbmHeader(format, width, height)
	}
	
	private def readLine(implicit in: InputStream): String = {
		var out = ""
		var b: Int = in.read
		while (b != 0xA) {
			out += b.toChar
			b = in.read
		}
		out
	}
	
	private case class PbmHeader(format: String, width: Int, height: Int)
	
}
