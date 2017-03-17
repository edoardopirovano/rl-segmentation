package org.edoardo.parser

import java.io.InputStream

abstract class Parser {
	def checkLineIs(value: String)(implicit in: InputStream): Unit = {
		val line: String = readLine
		if (line != value)
			throw new IllegalArgumentException("File is not a valid: Expected " + value + " but got " + line)
	}
	
	def readLine(implicit in: InputStream): String = {
		var out = ""
		var b: Int = in.read
		while (b != 0xA) {
			out += b.toChar
			b = in.read
		}
		out
	}
}