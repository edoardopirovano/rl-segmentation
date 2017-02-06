package org.edoardo.rl

import scala.collection.concurrent.TrieMap
import scala.util.Random

/**
  * Class to represent a state.
  *
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
	  * Return a random action with probability 1/epsilonReciprocal or the greedy action otherwise.
	  *
	  * @param state             The state to consider.
	  * @param epsilonReciprocal The reciprocal of epsilon that we want.
	  * @return The action chosen.
	  */
	def epsilonSoft(state: S, epsilonReciprocal: Int): A = if (Random.nextInt(epsilonReciprocal) == 0) randomPlay(state) else greedyPlay(state)
	
	def weakeningEpsilonSoft(state: S, epsilonReciprocal: Int): A = {
		val adjustedEpsilon: Long = epsilonReciprocal * (1 + timesStateSeen(state))
		if (adjustedEpsilon < Integer.MAX_VALUE) epsilonSoft(state, adjustedEpsilon.toInt) else greedyPlay(state)
	}
		
	
	def timesStateSeen(state: S): Long = values.get(state).map(seen => seen.values.map(_._2).sum).getOrElse(0)
	
	/**
	  * Choose a random action to play.
	  *
	  * @return A random action.
	  */
	def randomPlay(state: S): A = Random.shuffle(addStateIfMissing(state).keys).head
	
	/**
	  * Choose the greedy action to play.
	  *
	  * @param state The state to consider.
	  * @return The current greedy action from the given state.
	  */
	def greedyPlay(state: S): A = addStateIfMissing(state).maxBy(_._2._1)._1
	
	/**
	  * Add a state to the mapping if doesn't already exist.
	  *
	  * @param state The state to add to the mapping.
	  * @return The corresponding object for its estimated value.
	  */
	private def addStateIfMissing(state: S): TrieMap[A, (BigDecimal, Long)] = values.getOrElseUpdate(state, {
		val result: TrieMap[A, (BigDecimal, Long)] = new TrieMap()
		for (a <- state.getAll)
			result += ((a, (BigDecimal(0.0), 0L)))
		result
	})
	
	/**
	  * Update the policy by adding an observed reward for a given play.
	  *
	  * @param state  The state the play was made from.
	  * @param action The action performed.
	  * @param reward The reward obtained.
	  */
	def update(state: S, action: A, reward: Double): Unit = {
		var map: TrieMap[A, (BigDecimal, Long)] = addStateIfMissing(state)
		val old: (BigDecimal, Long) = map(action)
		map += ((action, (((old._1 * old._2) + reward) / (old._2 + 1), old._2 + 1)))
		values += ((state, map))
	}
}