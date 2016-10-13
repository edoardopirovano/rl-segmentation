package org.edoardo

import scala.collection.concurrent.TrieMap
import scala.util.Random

/**
  * Class to represent a state.
  * @tparam T Class of the actions that are performed from this state.
  */
abstract class State[T <: Action] {
	def getAll: List[T]
}

/**
  * Trait to represent an action.
  */
trait Action

/**
  * Class to represent a policy.
  */
class Policy[A <: Action, S <: State[A]] {
	/**
	  * Create a mapping for storing estimated values.
	  */
	var values: TrieMap[S, TrieMap[A, (BigDecimal, Long)]] = TrieMap()
	
	/**
	  * Add a state to the mapping if doesn't already exist.
	  * @param state The state to add to the mapping.
	  * @return The corresponding object for its estimated value.
	  */
	def addStateIfMissing(state: S): TrieMap[A, (BigDecimal, Long)] = values.getOrElseUpdate(state, {
		val result : TrieMap[A, (BigDecimal, Long)] = new TrieMap()
		for (a <- state.getAll)
			result += ((a, (BigDecimal(0.0), 0L)))
		result
	})
	
	/**
	  * Choose a random action to play.
	  * @return A random action.
	  */
	def randomPlay(state: S): A = Random.shuffle(addStateIfMissing(state).keys).head
	
	/**
	  * Choose the greedy action to play.
	  * @param state The state to consider.
	  * @return The current greedy action from the given state.
	  */
	def greedyPlay(state: S): A = addStateIfMissing(state).maxBy(_._2._1)._1
	
	/**
	  * Return a random action with probability 1/epsilonReciprocal or the greedy action otherwise.
	  * @param state The state to consider.
	  * @param epsilonReciprocal The reciprocal of epsilon that we want.
	  * @return The action chosen.
	  */
	def epsilonSoft(state: S, epsilonReciprocal: Int): A = if (Random.nextInt(epsilonReciprocal) == 0) randomPlay(state) else greedyPlay(state)
	
	/**
	  * Update the policy by adding an observed reward for a given play.
	  * @param state The state the play was made from.
	  * @param action The action performed.
	  * @param reward The reward obtained.
	  */
	def update(state: S, action: A, reward: Int) = {
		var map = addStateIfMissing(state)
		val old = map(action)
		map += ((action, (((old._1 * old._2) + reward) / (old._2 + 1), old._2 + 1)))
		values += ((state, map))
	}
}