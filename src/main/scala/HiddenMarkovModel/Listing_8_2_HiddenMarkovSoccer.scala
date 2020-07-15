package HiddenMarkovModel



import com.cra.figaro.algorithm.factored.VariableElimination
import com.cra.figaro.language._
import com.cra.figaro.library.compound.If



import org.scalactic.TripleEquals._
import org.scalactic.Tolerance._

/**
 * Source: from Avi Pfeffer book
 */

/**
 * (key concept) hidden markov model (HMM) = https://synergo.atlassian.net/wiki/spaces/KnowRes/pages/34407064/hidden+Markov+model+HMM
 */


object Listing_8_2_HiddenMarkovSoccer extends App {

	// Length of the chain in time steps
	val chainTimeStepLength: Int = 90


	// Confident: vector of hidden state variables, one for each time step. Indicates if we are confident of having
	// the ball, for each time step.
	val confidentVar: Array[Element[Boolean]] = Array.fill(chainTimeStepLength)(Constant(false))
	// Observation: vector of observation variables, one for each time step. Indicates if the team has possession of
	// the soccer ball, at each time step.
	val possessionVar: Array[Element[Boolean]] = Array.fill(chainTimeStepLength)(Constant(false))


	// Sets the distribution of the initial hidden state of the sequence.
	confidentVar(0) = Flip(0.4)


	/**
	 * (key concept) transition model = https://synergo.atlassian.net/wiki/spaces/KnowRes/pages/2016641067/transition+model
	 *
	 * Transition model defining a distribution for each hidden state variable based on the value of the previous state
	 * variable in the sequence of the Markov chain.
	 *
	 * If you did have possession at previous time point, you continue at next time step to have it with probability	0.6, but if you  didn't have possession, you have possession at next time step only with a lower probability of 0.3
	 */

	for { minute <- 1 until chainTimeStepLength}{
		confidentVar(minute) = If(confidentVar(minute - 1), Flip(0.6), Flip(0.3))
	}


	/**
	 * (key concept) observation model = https://synergo.atlassian.net/wiki/spaces/KnowRes/pages/2029551779/observation+model
	 *
	 * Observation model defining a distribution for each observation variable given its corresponding hidden state
	 * variable.
	 */
	for {minute <- 0 until chainTimeStepLength }{
		possessionVar(minute) = If(confidentVar(minute), Flip(0.7), Flip(0.3))
	}

	// ------------------------------------------------------------------------------------------------------------------------------

	{
		/**
		 * Basic run-through example of HMM
		 *
		 * NOTE: The observations are cumulative, every observation (possession) adds something to your belief that you
		 * are confident (hidden state) at a time step.
		 */
		println("Probability we are confident at time step t = 2 ...\n")
		println("Prior probability: " + VariableElimination.probability(confidentVar(2), true))

		possessionVar(2).observe(true)
		println("After observing CURRENT possession of ball at t = 2: " +
			VariableElimination.probability(confidentVar(2), true))

		possessionVar(1).observe(true)
		println("After observing PREVIOUS possession of ball at t = 1: " +
			VariableElimination.probability(confidentVar(2), true))

		possessionVar(1).unobserve()
		println("After UNobserving PREVIOUS possession of ball at t = 1: " +
			VariableElimination.probability(confidentVar(2), true))

		possessionVar(1).observe(true)
		println("(again for me) After observing PREVIOUS possession of ball at t = 1: " +
			VariableElimination.probability(confidentVar(2), true))

		possessionVar(0).observe(true)
		println("After observing PREVIOUS possession of ball at t = 0: " +
			VariableElimination.probability(confidentVar(2), true))

		possessionVar(3).observe(true)
		println("After observing FUTURE possession of ball at t = 3: " +
			VariableElimination.probability(confidentVar(2), true))

		possessionVar(4).observe(true)
		println("After observing FUTURE possession of ball at t = 4: " +
			VariableElimination.probability(confidentVar(2), true))
	}


	// ------------------------------------------------------------------------------------------------------------------------------

	/**
	 * GOAL: querying the markov model for the probability distribution over the hidden state variable
	 * Confident at any time point, given observations at any time points.
	 */

	{
		/**
		 * Test 1: manipulating just the observed variable, not the hidden variable, and one time step in the past, not
		 * more time steps in the past
		 *
		 * Hidden state variable (to query) = confident
		 * Observation state variable (to set evidence) = possession
		 */

		val obsPriorProb = VariableElimination.probability(confidentVar(5), true)


		possessionVar(4).observe(true)
		//Now the probability of possession at time = 5 has not changed, contrary to previous simple markov model
		val obsProbAfter4 = VariableElimination.probability(confidentVar(5), true)


		println(s"obsPriorProb = $obsPriorProb")
		println(s"obsProbAfter4 = $obsProbAfter4")
		/**
		 * Test 2: manipulating just the hidden state variable, not the observed variable , and one time step in the past.
		 */
		possessionVar(4).unobserve()

		confidentVar(4).observe(true)

		val hiddenProbAfter4 = VariableElimination.probability(confidentVar(5), true)

		println(s"hiddenProbAfter4 = $hiddenProbAfter4")
	}



	{
		/**
		 * Example 2 of Markov Assumption: (subtle)
		 *
		 * Possession at any time point (5) depends only on possession at time step (4) and is independent of
		 * possessions at previous time points.
		 */
		//Recording the prior probability of havging possession at time step 5
		val priorProb: Double = VariableElimination.probability(confidentVar(5), true)

		//Setting an earlier, not directly previous, state
		possessionVar(3).observe(true)
		val probAfter3: Double = VariableElimination.probability(confidentVar(5), true)

		possessionVar(2).observe(false)
		val probAfter2: Double = VariableElimination.probability(confidentVar(5), true)


		/*assert(priorProb != probAfter3, "Test 1 Markov: prior probability of possession at time step 5 need not " +
			"equal " +
			"probability of possession at time step 5 after observing possession at time step 3")*/

		/*assert(priorProb != probAfter2, "Test 1 Markov: prior probability of possession at time step 5 need not " +
			"equal " +
			"probability of possession at time step 5 after observing possession at time step 2")*/


		println(s"\nPrior probability of possession at t = 5: " + priorProb)

		println(s"Probability of possession at t = 5 after observing possession at t = 2: $probAfter2")

		println(s"Probability of possession at t = 5 after observing possession at t = 2, 3: $probAfter3")


		//TODO this doesn't hold for HMM only for markov why???
		/*assert(probAfter3 === probAfter2 +- 0.00000000001, "Test (past) Markov Assumption: probability of possession at " +
			"time step 5 is not affected by whether possession occurred at previous time steps")*/
	}


	{
		/**
		 * Example 2 of Markov Assumption:
		 *
		 * Possession true for two previous time steps in a row --- same probability of possession for time step 5
		 * because the new observation hasn't change the probability.
		 * This is due to the Markov Assumption (the state at any time point (5) is conditionally independent of all
		 * earlier states (3, 4) given the directly previous state (4) )
		 */

		val priorProb: Double = VariableElimination.probability(confidentVar(5), true)
		// Setting possession at time steps 3 and 4.
		possessionVar(4).observe(true)
		val probAfter4: Double = VariableElimination.probability(confidentVar(5), true)

		possessionVar(3).observe(true)
		val probAfter3_edgeCase = VariableElimination.probability(confidentVar(5), true)

		println(s"\nPrior probability of possession at t = 5: $priorProb")
		println(s"Probability of possession at t = 5 after observing possession at t = 3: $probAfter3_edgeCase")
		println(s"Probability of possession at t = 5 after observing possession at t = 3, 4: $probAfter4")

		assert(probAfter4 === probAfter3_edgeCase +- 0.000000000001, "Test (past) Markov Assumption")
	}




	{
		/**
		 * Example 3 markov assumption
		 *
		 * Whether you had possession in minute 7 adds no new information to possession at minute 5 after minute 6 is
		 * known.
		 *
		 */
		val priorProb = VariableElimination.probability(confidentVar(5), true)

		//Adding information: we had possession at time step 6
		possessionVar(6).observe(true)
		val probAfter6: Double = VariableElimination.probability(confidentVar(5), true)

		//Adding future information again: possession at time step 7
		possessionVar(7).observe(true)
		val probAfter7: Double = VariableElimination.probability(confidentVar(5), true)

		println(s"\nPrior probability of possession at t = 5: $priorProb")
		println(s"Probability of possession at t = 5 after observing possession at t = 6: $probAfter6")
		println(s"Probability of possession at t = 5 after observing possession at t = 6, 7: $probAfter7")

		//TODO this doesn't hold for the HMM --- why?
		//assert(probAfter6 === probAfter7 +- 0.000000000001, "Test (future) markov assumption")
	}


	//TODO take the above example tests and compare to ordinary model, to see if I can create a test case inheriting
	// from markov for this HMM suite (question to answer: is HMM a kind-of markov model? If so then it should be
	// able to inherit its test scenarios and pass them all)
}
