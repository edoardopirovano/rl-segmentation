package org.edoardo.segmentation

import org.edoardo.rl.{Action, State}

/**
  * Represents the possible actions for agent.
  * @param include whether the action was to add a region or exclude it
  */
case class Decision(include: Boolean) extends Action

/**
  * Represents the possible states for the agent.
  * @param info the information corresponding to region on which the agent should base its decision to include or exclude
  */
case class RegionInfo(info: List[Int]) extends State[Decision] {
	/**
	  * Gives the actions available for a region, which are always to include or exclude it.
	  * @return the actions available for a region
	  */
	override def getAll: List[Decision] = List(Decision(true), Decision(false))
}