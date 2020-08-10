package MarkovChain

import com.cra.figaro.algorithm.factored.VariableElimination
import com.cra.figaro.language._
import com.cra.figaro.library.compound.If



/**
 * Source: from Avi Pfeffer book
 */
object Listing_8_1_MarkovChain {


	var CHAIN_LENGTH: Int = 90

	/**
	 * Creates a markov chain represention possession of soccer ball at discrete time steps
	 *
	 * @param length length of the markov chain in discrete time steps
	 * @return
	 */
	def createMarkovChain(length: Int ): Array[Element[Boolean]] = {

		/**
		 * (key concept) Markov Chain = https://synergo.atlassian.net/wiki/spaces/KnowRes/pages/2028044306/Markov+model
		 *
		 * Is the boolean state variable Possession, which has time states, and indicates whether your team has
		 * possession of the soccer ball, for ever ytime point from 0 through 89.
		 */
		val markovRandomVariable: Array[Element[Boolean]] = Array.fill(length)(Constant(false))

		// Sets the distribution of the initial state of the sequence.
		// Distribution over whether your team has possession at time point 0.
		markovRandomVariable(0) = Flip(0.5)


		/**
		 * (key concept) transition model = https://synergo.atlassiannet/wiki/spaces/KnowRes/pages/2016641067/transition+model
		 *
		 * Transition model defining a distribution for each state variable based on the value of the previous state
		 * variable in the sequence of the Markov chain.
		 * If you did have possession at previous time point, you continue at next time step to have it with probability
		 * 0.6, but if you  didn't have possession, you have possession at next time step only with a lower probability of 0.3
		 */
		for {minute <- 1 until length} {
			markovRandomVariable(minute) = If(test = markovRandomVariable(minute - 1), thn = Flip(0.6), els = Flip(0.3))
		}

		markovRandomVariable
	}

}

object Listing_8_1_MarkovSoccerChainRunner extends App {

	import Listing_8_1_MarkovChain._


	// GOAL: querying the markov model for the probability distribution over the state variable Possession at
	// any time point, given observations at any time points.


	val possessionVar: Array[Element[Boolean]] = createMarkovChain(length = CHAIN_LENGTH)


	/**
	 * 1) First ask for probability before observing any evidence:
	 */
	// Query probability that you have possession at time step 5, before observing any evidence:
	// This is also called the prior probability that you have possession at time step 5.
	println(VariableElimination.probability(target = possessionVar(5), value = true))

	/**
	 * 2) Now set evidence of having possession at time step 4
	 */
	possessionVar(4).observe(true)
	//Now the probability of possession at time = 5 has increased
	println(VariableElimination.probability(possessionVar(5), true))

	/**
	 * Repeating query doesn't change result from previous query: still assumes you had the ball at time step 4.
	 */
	println(VariableElimination.probability(possessionVar(5), true))

	/**
	 * Having no ball at time step 4 lowers the probability of having it at time step 5
	 */
	possessionVar(4).observe(false)
	println(VariableElimination.probability(possessionVar(5), true))
}