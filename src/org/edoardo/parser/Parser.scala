package org.edoardo.parser

import java.io.InputStream

/**
  * Abstract class containing a few helper methods used in classes that implement Parsers.
  */
abstract class Parser {
	/**
	  * Read a line and check it has a certain value (throwing an exception if this is not the case).
	  * @param value the value the line should have
	  * @param in the input stream to read the line from
	  */
	def checkLineIs(value: String)(implicit in: InputStream): Unit = {
		val line: String = readLine
		if (line != value)
			throw new IllegalArgumentException("File is not a valid: Expected " + value + " but got " + line)
	}
	
	/**
	  * Read a line from an input stream
	  * @param in the input stream to read from
	  * @return the line read
	  */
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