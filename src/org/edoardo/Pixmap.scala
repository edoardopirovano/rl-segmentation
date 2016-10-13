package org.edoardo

import java.awt.Color
import java.io.{BufferedInputStream, BufferedOutputStream, FileInputStream, FileOutputStream, InputStream}

object Pixmap {
	
	private case class PbmHeader(format: String, width: Int, height: Int)
	
	def load(filename: String): Option[RgbBitmap] = {
		implicit val in = new BufferedInputStream(new FileInputStream(filename))
		
		val header = readHeader
		
		if (header.format == "P1") {
			// Black and white
			val bm = new RgbBitmap(header.width, header.height)
			
			for (y <- 0 until bm.height) {
				val line = readLine
				val parts = line.split("\\s")
				for (x <- 0 until bm.width) {
					if (parts(x).equals("1"))
						bm.setPixel(x, y, new Color(0, 0, 0))
					else
						bm.setPixel(x, y, new Color(255, 255, 255))
				}
			}
			Some(bm)
		} else if (header.format == "P2") {
			// Greyscale
			val bm = new RgbBitmap(header.width, header.height)
			val range = readLine
			assert(range.toInt == 255)
			
			for (y <- 0 until bm.height) {
				val line = readLine
				val parts = line.split("\\s")
				for (x <- 0 until bm.width) {
					val color = parts(x).toInt
					bm.setPixel(x, y, new Color(color, color, color))
				}
			}
			Some(bm)
		} else None
	}
	
	def header(height: Int, width: Int) = "P1\n" + width + " " + height + "\n"
	
	def write(filename: String, bm: RgbBitmap) = {
		val s = new BufferedOutputStream(new FileOutputStream(filename))
		s.write(header(bm.height, bm.width).getBytes)
		for (y <- 0 until bm.height) {
			for (x <- 0 until bm.width) {
				if (bm.getPixel(x, y).equals(new Color(255, 255, 255)))
					s.write("0 ".getBytes)
				else
					s.write("1 ".getBytes)
			}
			s.write("\n".getBytes)
		}
		s.close()
	}
	
	def colorVal(v: Double): Int = {
		val scaled = v * 256
		if (scaled < 0) 0
		else if (scaled > 255) 255
		else scaled.toInt
	}
	
	private def readHeader(implicit in: InputStream) = {
		val format = readLine
		var line = readLine
		
		while (line.startsWith("#"))
			line = readLine
		
		val parts = line.split("\\s")
		val width = parts(0).toInt
		val height = parts(1).toInt
		
		PbmHeader(format, width, height)
	}
	
	private def readLine(implicit in: InputStream) = {
		var out = ""
		var b = in.read
		while (b != 0xA) {
			out += b.toChar; b = in.read
		}
		out
	}
}
