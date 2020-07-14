package MarkovChain



import com.cra.figaro.algorithm.factored.VariableElimination
import com.cra.figaro.language._
import com.cra.figaro.library.compound.If



import org.scalactic.TripleEquals._
import org.scalactic.Tolerance._

/**
 *
 */
object Listing_8_1_MarkovChainSoccer extends App {

	// Length of the chain in time steps
	val chainTimeStepLength: Int = 90


	/**
	 * (key concept) Markov Chain = https://synergo.atlassian.net/wiki/spaces/KnowRes/pages/2028044306/Markov+model
	 */
	// The boolean state variable Possession, which has time states, and indicates whether your team has possession
	// of the soccer ball, for ever ytime point from 0 through 89.
	val possessionVar: Array[Element[Boolean]] = Array.fill(chainTimeStepLength)(Constant(false))

	// Sets the distribution of the initial state of the sequence.
	// Distribution over whether your team has possession at time point 0.
	possessionVar(0) = Flip(0.5)


	/**
	 * (key concept) transition model = https://synergo.atlassian.net/wiki/spaces/KnowRes/pages/2016641067/transition+model
	 */
	//Transition model defining a distribution for each state variable based on the value of the previous state
	// variable in the sequence of the Markov chain.
	// If you did have possession at previous time point, you continue at next time step to have it with probability
	// 0.6, but if you  didn't have possession, you have possession at next time step only with a lower probability of 0.3
	for { minute <- 1 until chainTimeStepLength}{
		possessionVar(minute) = If(test = possessionVar(minute - 1), thn = Flip(0.6), els = Flip(0.3))
	}



	// ------------------------------------------------------------------------------------------------------------------------------

	// GOAL: querying the markov model for the probability distribution over the state variable Possession at
	// any time point, given observations at any time points.


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


	/**
	 * Removing all evidences, back to original prior probability.
	 */
	possessionVar(4).unobserve()
	println(VariableElimination.probability(possessionVar(5), true))


	{
		/**
		 * Example 2 of Markov Assumption: (subtle)
		 *
		 * Possession at any time point (5) depends only on possession at time step (4) and is independent of
		 * possessions at previous time points.
		 */
		//Recording the prior probability of havging possession at time step 5
		var priorProb: Double = VariableElimination.probability(possessionVar(5), true)

		//Setting an earlier, not directly previous, state
		possessionVar(3).observe(true)
		val probAfter3: Double = VariableElimination.probability(possessionVar(5), true)

		possessionVar(2).observe(false)
		val probAfter2: Double = VariableElimination.probability(possessionVar(5), true)


		assert(priorProb != probAfter3, "Test 1 Markov: prior probability of possession at time step 5 need not equal " +
			"probability of possession at time step 5 after observing possession at time step 3")

		assert(priorProb != probAfter2, "Test 1 Markov: prior probability of possession at time step 5 need not equal " +
			"probability of possession at time step 5 after observing possession at time step 2")

		println(s"\nProbability of possession at t = 5 after observing possession at t = 2: $probAfter2")
		println(s"Probability of possession at t = 5 after observing possession at t = 2, 3: $probAfter3")

		assert(probAfter3 === probAfter2 +- 0.00000000001, "Test (past) Markov Assumption: probability of possession at " +
			"time step 5 is not affected by whether possession occurred at previous time steps")
	}



	//possessionVar(2).unobserve()
	//possessionVar(3).unobserve() // cleaning up state for this test below

	{
		/**
		 * Example 2 of Markov Assumption:
		 *
		 * Possession true for two previous time steps in a row --- same probability of possession for time step 5
		 * because the new observation hasn't change the probability.
		 * This is due to the Markov Assumption (the state at any time point (5) is conditionally independent of all
		 * earlier states (3, 4) given the directly previous state (4) )
		 */

		val priorProb: Double = VariableElimination.probability(possessionVar(5), true)
		// Setting possession at time steps 3 and 4.
		possessionVar(4).observe(true)
		val probAfter4: Double = VariableElimination.probability(possessionVar(5), true)

		possessionVar(3).observe(true)
		val probAfter3_edgeCase = VariableElimination.probability(possessionVar(5), true)

		println(s"\nProbability of possession at t = 5 after observing possession at t = 3: $probAfter3_edgeCase")
		println(s"Probability of possession at t = 5 after observing possession at t = 3, 4: $probAfter4")

		assert(probAfter4 === probAfter3_edgeCase +- 0.000000000001, "Test (past) Markov Assumption")
	}




	/*possessionVar(4).unobserve()
	possessionVar(3).unobserve() */// cleaning up state for this test below

	{
		/**
		 * Example 3 markov assumption
		 *
		 * Whether you had possession in minute 7 adds no new information to possession at minute 5 after minute 6 is
		 * known.
		 *
		 */
		//Adding information: we had possession at time step 6
		possessionVar(6).observe(true)
		val probAfter6: Double = VariableElimination.probability(possessionVar(5), true)

		//Adding future information again: possession at time step 7
		possessionVar(7).observe(true)
		val probAfter7: Double = VariableElimination.probability(possessionVar(5), true)

		println(s"\nProbability of possession at t = 5 after observing possession at t = 6: $probAfter6")
		println(s"Probability of possession at t = 5 after observing possession at t = 6, 7: $probAfter7")

		assert(probAfter6 === probAfter7 +- 0.000000000001, "Test (future) markov assumption")
	}
}
