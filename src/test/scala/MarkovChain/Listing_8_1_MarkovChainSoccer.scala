package MarkovChain



import com.cra.figaro.algorithm.factored.VariableElimination
import com.cra.figaro.language._



import org.scalactic.TripleEquals._
import org.scalactic.Tolerance._

import org.specs2.mutable._


class MarkovChainTests extends Specification {



	import Listing_8_1_MarkovChainSoccer._

	val possessionVar: Array[Element[Boolean]] = createMarkovSoccerChain(length = 90)

	final val FIVE: Int = 5
	final val FOUR: Int = 4
	final val THREE: Int = 3
	final val TWO: Int = 2
	final val SIX: Int = 6
	final val SEVEN: Int = 7

	//---------------------------------------------------------------------------------------------------------------------

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



	// ---------------------------------------------------------------------------------------------------------------------------
	//import org.specs2.mutable._
	import org.scalacheck.Arbitrary


	//Recording the prior probability of havging possession at time step 5
	var priorProb: Double = VariableElimination.probability(possessionVar(5), true)
	println(s"\nPrior probability of possession at time t = 5:  \t $priorProb")

	{
		/**
		 * Example of Markov Assumption for Non-Immediate Past
		 *
		 * Possession at any time point (5) depends only on possession at time step (4) and is independent of
		 * possessions at previous time points.
		 */


		//Setting an earlier, not directly previous, state
		possessionVar(3).observe(true)
		val probAfter3: Double = VariableElimination.probability(possessionVar(5), true)

		possessionVar(2).observe(false)
		val probAfter2: Double = VariableElimination.probability(possessionVar(5), true)


		assert(priorProb != probAfter3, "Test 1 Markov: prior probability of possession at time step 5 need not equal " +
			"probability of possession at time step 5 after observing possession at time step 3")

		assert(priorProb != probAfter2, "Test 1 Markov: prior probability of possession at time step 5 need not equal " +
			"probability of possession at time step 5 after observing possession at time step 2")


		println(s"\nProbability of possession at t = 5 after observing possession at t = 2: \t $probAfter2")
		println(s"Probability of possession at t = 5 after observing possession at t = 2, 3: \t $probAfter3")


		assert(probAfter3 === probAfter2 +- 0.00000000001, "Test (past) Markov Assumption: probability of possession at " +
			"time step 5 is not affected by whether possession occurred at previous time steps")
	}



	{
		/**
		 * Example of Markov Assumption for Immediate Past
		 *
		 * Possession true for two previous time steps in a row --- same probability of possession for time step 5
		 * because the new observation hasn't change the probability.
		 * This is due to the Markov Assumption (the state at any time point (5) is conditionally independent of all
		 * earlier states (3, 4) given the directly previous state (4) )
		 */


		// Setting possession at time steps 3 and 4.
		possessionVar(4).observe(true)
		val probAfter4: Double = VariableElimination.probability(possessionVar(5), true)

		possessionVar(3).observe(true)
		val probAfter3_edgeCase = VariableElimination.probability(possessionVar(5), true)

		println(s"\nProbability of possession at t = 5 after observing possession at t = 3: \t $probAfter3_edgeCase")
		println(s"Probability of possession at t = 5 after observing possession at t = 3, 4: \t $probAfter4")

		assert(probAfter4 === probAfter3_edgeCase +- 0.000000000001, "Test (past) Markov Assumption")
	}




	/*possessionVar(4).unobserve()
	possessionVar(3).unobserve() */// cleaning up state for this test below

	{
		/**
		 * Example of Markov Assumption for Future
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

		println(s"\nProbability of possession at t = 5 after observing possession at t = 6: \t $probAfter6")
		println(s"Probability of possession at t = 5 after observing possession at t = 6, 7: \t $probAfter7")

		assert(probAfter6 === probAfter7 +- 0.000000000001, "Test (future) markov assumption")
	}
}
