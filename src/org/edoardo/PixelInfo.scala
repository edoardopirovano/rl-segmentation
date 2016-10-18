package org.edoardo

case class Decision(include: Boolean) extends Action

case class PixelInfo(gradMagnitude: Int, luminosity: Int) extends State[Decision] {
	override def getAll: List[Decision] = List(Decision(true), Decision(false))
}