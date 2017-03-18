package org.edoardo.parser

import java.io.{BufferedInputStream, FileInputStream, InputStream}

import scala.collection.mutable

/**
  * Describes a pixel (leaf node) of the IPF.
  */
case class PixelProperties(baseValue: Int, gradientMagnitude: Int, greyValue: Int, parent: Int)

/**
  * Describes a branch node of the IPF.
  */
case class Node(centroid: (Float, Float, Float), maxGrey: Int, meanGrey: Float, minGrey: Int, xMin: Int, yMin: Int,
				zMin: Int, xMax: Int, yMax: Int, zMax: Int, voxelCount: Int, children: List[Int], parent: Int)

/**
  * Describes the leaf layer of an IPF.
  */
case class LeafLayer(sizeX: Int, sizeY: Int, sizeZ: Int, pixelInfo: Array[Array[Array[PixelProperties]]],
					 regionToPixels: mutable.Map[Int, List[(Int, Int, Int)]])

/**
  * Describes a branch layer of an IPF.
  */
case class BranchLayer(nodes: mutable.Map[Int, Node], edges: mutable.Map[Int, List[(Int, Int)]])

/**
  * Describes an IPF and contains helper methods for accessing useful properties of it.
  * @param width the width of the image
  * @param height the height of the image
  * @param depth the depth of the image
  * @param leafLayer the leaf layer of the IPF
  * @param branchLayers the branch layers of the IPF
  */
case class VolumeIPF(width: Int, height: Int, depth: Int, leafLayer: LeafLayer, branchLayers: List[BranchLayer]) {
	/**
	  * Get all pixels in a region, which is assumed to be in the first branch layer.
	  * @param region the identifier for the region
	  * @return a list of pixels in the region
	  */
	def getRegionPixels(region: Int): List[(Int, Int, Int)] = {
		leafLayer.regionToPixels(region)
	}
	
	/**
	  * Get all pixels in a region, in the given layer
	  * @param layer the layer of the IPF the region is in
	  * @param region the identifier for the region
	  * @return a list of pixels in the region
	  */
	def getRegionPixels(layer: Int, region: Int): List[(Int, Int, Int)] = {
		var regions: List[Int] = List(region)
		for (i <- 0 until (layer - 1)) {
			var newRegions: List[Int] = List()
			for (region <- regions)
				newRegions = newRegions ++ branchLayers(branchLayers.length - layer + i).nodes(region).children
			regions = newRegions
		}
		regions.flatMap(region => getRegionPixels(region))
	}
	
	/**
	  * For a given seed point and layer, find the corresponding region and then return a list of all layer 1 children
	  * of this seed region (used to initialise a selection).
	  * @param layer the layer to look in
	  * @param x the x coordinate of the seed
	  * @param y the y coordinate of the seed
	  * @param z the z coordinate of the seed
	  * @return a list of layer 1 children of the selected seed reigon
	  */
	def getRegionsInLayer(layer: Int, x: Int, y: Int, z: Int): List[Int] = {
		if (layer == 1) return List(leafLayer.pixelInfo(x)(y)(z).parent)
		var regions: List[Int] = getRegionInLayer(layer, x, y, z).children
		for (i <- branchLayers.length - layer + 1 until branchLayers.length - 1)
			regions = regions.flatMap(r => branchLayers(i).nodes(r).children)
		regions
	}
	
	/**
	  * Find which region contains a point in a given layer of the IPF.
	  * @param layer the layer to look in
	  * @param x the x coordinate to look at
	  * @param y the y coordinate to look at
	  * @param z the z coordinate to look at
	  * @return the identifier corresponding to the region at (x, y, z) in the given layer
	  */
	def getRegionInLayer(layer: Int, x: Int, y: Int, z: Int): Node = {
		var region: Node = branchLayers.last.nodes(leafLayer.pixelInfo(x)(y)(z).parent)
		for (i <- 2 to layer)
			region = branchLayers(branchLayers.length - i).nodes(region.parent)
		region
	}
	
	/**
	  * Find all neighbours of a region (assumed to be in the last branch layer).
	  * @param region the region identifier
	  * @return a list of region identifiers of neighbouring regions
	  */
	def getNeighbours(region: Int): List[Int] = {
		branchLayers.last.edges(region).map(edge => edge._1)
	}
	
	/**
	  * Find the z coordinate of a given region (assumed to be in the last branch layer).
	  * Note this assumes the IPF is an axial one (ie. has been made with separate forests for each X-Y slice).
	  * @param region the region identifier
	  * @return the z coordinate of the region
	  */
	def getZ(region: Int): Int = {
		getRegionPixels(region).head._3
	}
}

/**
  * Implements a parser for an IPF.
  */
object IPF extends Parser {
	
	/**
	  * Read an IPF from a file.
	  * @param fileName the file to read from
	  * @return the IPF in the file
	  */
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
				nodes.put(node, Node((centroid(0).toFloat, centroid(1).toFloat, centroid(2).toFloat),
					properties(1).toInt, properties(2).toFloat, properties(3).toInt, properties(4).toInt,
					properties(5).toInt, properties(6).toInt, properties(7).toInt, properties(8).toInt,
					properties(9).toInt, properties(10).toInt, children, parent))
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
				edges.put(fromAndTo(0).toInt,
					(fromAndTo(1).toInt, split(1).toInt) :: edges.getOrElseUpdate(fromAndTo(0).toInt, List()))
				edges.put(fromAndTo(1).toInt,
					(fromAndTo(0).toInt, split(1).toInt) :: edges.getOrElseUpdate(fromAndTo(1).toInt, List()))
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
				pixelProperties(x)(y).update(z,
					PixelProperties(properties(0).toInt, properties(1).toInt, properties(2).toInt, parent))
				parentRegionToPixels.put(parent, (x, y, z) :: parentRegionToPixels.getOrElseUpdate(parent, List()))
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
}

