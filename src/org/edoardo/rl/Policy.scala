package org.edoardo.rl

import scala.collection.concurrent.TrieMap
import scala.util.Random

/**
  * Abstract class to represent a state.
  * @tparam T class of the actions that are performed from this state
  */
abstract class State[T <: Action] {
	def getAvailableActions: List[T]
}

/**
  * Trait to represent an action.
  */
trait Action

/**
  * Class to represent a policy.
  * @tparam A the type of actions the agent can perform under this policy
  * @tparam S the type of states the agent can encounter under this policy
  */
class Policy[A <: Action, S <: State[A]] {
	/**
	  * Create a mapping for storing estimated values.
	  */
	var values: TrieMap[S, TrieMap[A, (BigDecimal, Long)]] = TrieMap()
	
	/**
	  * Return a random action with probability 1/epsilonReciprocal or the greedy action otherwise.
	  * @param state the state to consider
	  * @param epsilonReciprocal the reciprocal of epsilon that we want
	  * @return the action chosen
	  */
	def epsilonSoft(state: S, epsilonReciprocal: Int): A =
		if (Random.nextInt(epsilonReciprocal) == 0) randomPlay(state)
		else greedyPlay(state)
	
	/**
	  * Choose a random action to play.
	  * @return a random action
	  */
	def randomPlay(state: S): A = Random.shuffle(state.getAvailableActions).head
	
	/**
	  * Choose the greedy action to play.
	  * @param state the state to consider
	  * @return the current greedy action from the given state
	  */
	def greedyPlay(state: S): A = {
		if (!haveEncountered(state)) state.getAvailableActions.head
		else values(state).maxBy(_._2._1)._1
	}
	
	/**
	  * Update the policy by adding an observed reward for a given play.
	  * @param state the state the play was made from
	  * @param action the action performed
	  * @param reward the reward obtained
	  */
	def update(state: S, action: A, reward: Double): Unit = {
		var map: TrieMap[A, (BigDecimal, Long)] = values.getOrElseUpdate(state, {
			val result: TrieMap[A, (BigDecimal, Long)] = new TrieMap()
			for (a <- state.getAvailableActions)
				result += ((a, (BigDecimal(0.0), 0L)))
			result
		})
		val old: (BigDecimal, Long) = map(action)
		map += ((action, (((old._1 * old._2) + reward) / (old._2 + 1), old._2 + 1)))
		values += ((state, map))
	}
	
	/**
	  * Check if a state has been encountered before.
	  * @param state the state to check
	  * @return whether or not we have seen this state  before
	  */
	def haveEncountered(state: S): Boolean = {
		if (values.get(state).isDefined) true
		else false
	}
	
	/**
	  * Clear the policy, forgetting everything learnt.
	  */
	def clear(): Unit = {
		values = TrieMap()
	}
}