package MarkovChain


import com.cra.figaro.algorithm.factored.VariableElimination
import com.cra.figaro.language._
import com.cra.figaro.library.compound.If



/**
 * Source: from Avi Pfeffer book
 */
object Listing_8_1_MarkovChainSoccer {

	/**
	 * Creates a markov chain represention possession of soccer ball at discrete time steps
	 *
	 * @param length length of the markov chain in discrete time steps
	 * @return
	 */
	def createMarkovSoccerChain(length: Int ): Array[Element[Boolean]] = {

		/**
		 * (key concept) Markov Chain = https://synergo.atlassian.net/wiki/spaces/KnowRes/pages/2028044306/Markov+model
		 *
		 * Is the boolean state variable Possession, which has time states, and indicates whether your team has
		 * possession of the soccer ball, for ever ytime point from 0 through 89.
		 */
		val possessionVar: Array[Element[Boolean]] = Array.fill(length)(Constant(false))

		// Sets the distribution of the initial state of the sequence.
		// Distribution over whether your team has possession at time point 0.
		possessionVar(0) = Flip(0.5)


		/**
		 * (key concept) transition model = https://synergo.atlassiannet/wiki/spaces/KnowRes/pages/2016641067/transition+model
		 *
		 * Transition model defining a distribution for each state variable based on the value of the previous state
		 * variable in the sequence of the Markov chain.
		 * If you did have possession at previous time point, you continue at next time step to have it with probability
		 * 0.6, but if you  didn't have possession, you have possession at next time step only with a lower probability of 0.3
		 */
		for {minute <- 1 until length} {
			possessionVar(minute) = If(test = possessionVar(minute - 1), thn = Flip(0.6), els = Flip(0.3))
		}

		possessionVar
	}

}