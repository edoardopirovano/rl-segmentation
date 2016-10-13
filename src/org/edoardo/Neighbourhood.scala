package org.edoardo

import java.awt.Color

case class Decision(include: Boolean) extends Action

object Status extends Enumeration {
	val Selected, NotSelected, Pending, Unknown = Value
}

case class Cell(color: Color, status: Status.Value)

case class Neighbourhood(contents: List[List[Cell]]) extends State[Decision] {
	override def getAll: List[Decision] = List(Decision(true), Decision(false))
}
