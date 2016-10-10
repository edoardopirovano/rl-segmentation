package org.edoardo

import java.awt.Color
import java.io._

object Pixmap {
  private case class PbmHeader(format: String, width: Int, height: Int)

  def load(filename:String):Option[RgbBitmap] = {
    implicit val in = new BufferedInputStream(new FileInputStream(filename))

    val header = readHeader

    if(header.format == "P1") {
      val bm = new RgbBitmap(header.width, header.height)

      for(y <- 0 until bm.height; x <- 0 until bm.width; isBlack = in.read) {
        if (isBlack == 1)
          bm.setPixel(x, y, new Color(255, 255, 255))
        else
          bm.setPixel(x, y, new Color(0, 0, 0))
      }
      Some(bm)
    } else None
  }

  private def readHeader(implicit in: InputStream) = {
    val format = readLine
    var line = readLine

    while(line.startsWith("#"))
      line = readLine

    val parts = line.split("\\s")
    val width = parts(0).toInt
    val height = parts(1).toInt

    PbmHeader(format, width, height)
  }

  private def readLine(implicit in:InputStream) = {
    var out = ""
    var b = in.read
    while(b!=0xA){ out+=b.toChar; b=in.read }
    out
  }
}

object Main {
  val img = Pixmap.load("/home/edoardo/medimaging/datasource/Bitmaps/disc/discMS4.pbm").get

  def main(args: Array[String]): Unit = {
    println("Hello, world!")
  }
}
