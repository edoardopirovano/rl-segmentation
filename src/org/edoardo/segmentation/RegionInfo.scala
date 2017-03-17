package org.edoardo.segmentation

import org.edoardo.rl.{Action, State}

case class Decision(include: Boolean) extends Action

case class RegionInfo(info: List[Int]) extends State[Decision] {
	override def getAll: List[Decision] = List(Decision(true), Decision(false))
}