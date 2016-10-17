package org.edoardo

case class Decision(include: Boolean) extends Action

case class PixelInfo(xGradient: Int, yGradient: Int, luminosity: Int) extends State[Decision] {
	override def getAll: List[Decision] = List(Decision(true), Decision(false))
}