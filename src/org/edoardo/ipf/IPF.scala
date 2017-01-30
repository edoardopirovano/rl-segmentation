package org.edoardo.ipf

import java.io.{BufferedInputStream, FileInputStream, InputStream}

import scala.collection.mutable

case class PixelProperties(baseValue: Int, gradientMagnitude: Int, greyValue: Int, parent: Int)

case class Node(centroid: (Float, Float, Float), maxGrey: Int, meanGrey: Float, minGrey: Int, xMin: Int, yMin: Int,
				zMin: Int, xMax: Int, yMax: Int, zMax: Int, voxelCount: Int, children: List[Int], parent: Int)

case class VolumeIPF(width: Int, height: Int, depth: Int, leafLayer: LeafLayer, branchLayers: List[BranchLayer]) {
	def getRegionPixels(region: Int): List[(Int, Int)] = {
		leafLayer.regionToPixels(region).map(p => (p._1, p._2))
	}
	
	def getRegionsInLayer(layer: Int, x: Int, y: Int, z: Int): List[Int] = {
		if (layer == 1) return List(leafLayer.pixelInfo(x)(y)(z).parent)
		var regions: List[Int] = getRegionInLayer(layer, x, y, z).children
		for (i <- branchLayers.length - layer + 1 until branchLayers.length - 1)
			regions = regions.flatMap(r => branchLayers(i).nodes(r).children)
		regions
	}
	
	def getRegionInLayer(layer: Int, x: Int, y: Int, z: Int): Node = {
		var region: Node = branchLayers.last.nodes(leafLayer.pixelInfo(x)(y)(z).parent)
		for (i <- 2 to layer)
			region = branchLayers(branchLayers.length - i).nodes(region.parent)
		region
	}
	
	def getNeighbours(region: Int): List[Int] = {
		branchLayers.last.edges(region).map(edge => edge._1)
	}
}

case class LeafLayer(sizeX: Int, sizeY: Int, sizeZ: Int, pixelInfo: Array[Array[Array[PixelProperties]]],
					 regionToPixels: mutable.Map[Int, List[(Int, Int, Int)]])

case class BranchLayer(nodes: mutable.Map[Int, Node], edges: mutable.Map[Int, List[(Int, Int)]])

object IPF {
	def loadFromFile(fileName: String): VolumeIPF = {
		implicit val in = new BufferedInputStream(new FileInputStream(fileName))
		checkLineIs("VolumeIPF")
		checkLineIs("{")
		
		val sizeLine: Array[String] = readLine.drop(1).dropRight(1).split(", ")
		
		val leafLayer: LeafLayer = readLeafLayerSection()
		
		val topLayer: Int = readLine.toInt
		var branchLayers: List[BranchLayer] = List()
		for (i <- 1 to topLayer)
			branchLayers ::= readBranchLayer()
		
		checkLineIs("}")
		
		VolumeIPF(sizeLine(0).toInt, sizeLine(1).toInt, sizeLine(2).toInt, leafLayer, branchLayers)
	}
	
	private def readBranchLayer()(implicit in: InputStream): BranchLayer = {
		checkLineIs("ImageBranchLayer")
		checkLineIs("{")
		
		var stop = false
		val nodes: mutable.Map[Int, Node] = mutable.Map.empty
		while (!stop) {
			val line: String = readLine
			if (line == "|") stop = true
			else {
				val node: Int = line.toInt
				val properties: Array[String] = readLine.drop(2).dropRight(2).split("\\|")
				val centroid: Array[String] = properties(0).drop(1).dropRight(1).split(",")
				checkLineIs("{")
				var stopInner = false
				var children: List[Int] = List()
				while (!stopInner) {
					val innerLine: String = readLine
					if (innerLine == "}") stopInner = true
					else children ::= innerLine.toInt
				}
				val parent: Int = readLine.toInt
				nodes.put(node, Node((centroid(0).toFloat, centroid(1).toFloat, centroid(2).toFloat), properties(1).toInt,
					properties(2).toFloat, properties(3).toInt, properties(4).toInt, properties(5).toInt, properties(6).toInt,
					properties(7).toInt, properties(8).toInt, properties(9).toInt, properties(10).toInt, children, parent))
			}
		}
		
		val edges: mutable.Map[Int, List[(Int, Int)]] = mutable.Map.empty
		stop = false
		while (!stop) {
			val line: String = readLine
			if (line == "}") stop = true
			else {
				val split: Array[String] = line.drop(1).dropRight(1).split(", ")
				val fromAndTo: Array[String] = split(0).drop(1).dropRight(1).split(" ")
				edges.put(fromAndTo(0).toInt, (fromAndTo(1).toInt, split(1).toInt) :: edges.getOrElseUpdate(fromAndTo(0).toInt, List()))
				edges.put(fromAndTo(1).toInt, (fromAndTo(0).toInt, split(1).toInt) :: edges.getOrElseUpdate(fromAndTo(1).toInt, List()))
			}
		}
		
		BranchLayer(nodes, edges)
	}
	
	private def readLeafLayerSection()(implicit in: InputStream): LeafLayer = {
		checkLineIs("ImageLeafLayer")
		checkLineIs("{")
		val sizeX: Int = readLine.trim.split(" = ")(1).toInt
		val sizeY: Int = readLine.trim.split(" = ")(1).toInt
		val sizeZ: Int = readLine.trim.split(" = ")(1).toInt
		checkLineIs("|")
		
		var stop = false
		val pixelProperties: Array[Array[Array[PixelProperties]]] = Array.ofDim[PixelProperties](sizeX, sizeY, sizeZ)
		val parentRegionToPixels: mutable.Map[Int, List[(Int, Int, Int)]] = mutable.Map.empty
		var x = 0
		var y = 0
		var z = 0
		while (!stop) {
			val line: String = readLine
			if (line == "}") stop = true
			else {
				val properties: Array[String] = line.drop(1).dropRight(1).split("\\|")
				val parent: Int = readLine.toInt
				pixelProperties(x)(y).update(z, PixelProperties(properties(0).toInt, properties(1).toInt, properties(2).toInt, parent))
				parentRegionToPixels.put(parent, (x,y,z) :: parentRegionToPixels.getOrElseUpdate(parent, List()))
				x += 1
				if (x == sizeX) {
					x = 0
					y += 1
					if (y == sizeY) {
						y = 0
						z += 1
					}
				}
			}
		}
		
		LeafLayer(sizeX, sizeY, sizeZ, pixelProperties, parentRegionToPixels)
	}
	
	private def checkLineIs(value: String)(implicit in: InputStream): Unit = {
		val line: String = readLine
		if (line != value)
			throw new IllegalArgumentException("File is not a valid IPF: Expected " +
				value + " but got " + line)
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
}

