package org.edoardo

import java.awt.Color

case class Decision(include: Boolean) extends Action

case class Neighbourhood(contents: List[List[Color]]) extends State[Decision] {
	override def getAll: List[Decision] = List(Decision(true), Decision(false))
}
