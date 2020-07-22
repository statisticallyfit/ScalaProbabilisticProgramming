package MarkovChain



import com.cra.figaro.algorithm.factored.VariableElimination
import com.cra.figaro.language._


import org.scalatest._
import org.scalatest.featurespec.AnyFeatureSpec

//import org.scalatest.matchers.should.Matchers._
//import org.scalactic.TypeCheckedTripleEquals._
import org.scalactic.Tolerance._

//import org.specs2.mutable._




object MarkovChainSoccerState {


	import Listing_8_1_MarkovChainSoccer._

	val possessionVar: Array[Element[Boolean]] = createMarkovSoccerChain(length = 90)

	final val FIVE = 5
	final val FOUR = 4
	final val THREE: Int = 3
	final val TWO: Int = 2
	final val SIX: Int = 6
	final val SEVEN: Int = 7

	final val TOLERANCE: Double = 0.00000001
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




	//Recording the prior probability of havging possession at time step 5

//TODO print prior prob
	//println(s"\nPrior probability of possession at time t = $FIVE:  \t $priorProb")

}



class Listing_8_1_MarkovChainSoccerTests extends AnyFeatureSpec with GivenWhenThen {

	import MarkovChainSoccerState._



	info("Markov Assumption Test for Non-Immediate Past: ")
	info("Possession at current time t = 5")
	info("depends only on")
	info("possession at previous time point 4")
	info("and is independent of possessions at previous times.")



	Feature("How Markov Assumption Doesn't Apply for Non-Immediate Past") {

		Scenario("Observe possession at t = 2 (three steps behind current time t = 5)"){

			Given("no current observed ball possession at t = 5")
			val priorProb: Double = VariableElimination.probability(possessionVar(5), true)
			//assert()

			When("ball is possessed at t = 2")
			possessionVar(2).observe(true)
			val possessProbTWO: Double = VariableElimination.probability(possessionVar(5), true)


			Then("Markov assumption doesn't apply; the probability of possession at t = 5 differs from prior " +
				"probability of possession at t = 5")
			assert(possessProbTWO != priorProb)

			Console.println(s"\n(F1, S1) Prior probability of possession at t = 5: \t $priorProb")
			Console.println(s"(F1, S1) Probability of possession at t = 5 after observing possession at t = 2: \t " +
				s"$possessProbTWO")
		}


		Scenario("Observe possession at t = 2 and t = 3 (two steps behind current time t = 5)") {

			Given("no current observed ball possession at t = 5")
			val priorProb: Double = VariableElimination.probability(possessionVar(5), true)
			//assert()

			When("ball is possessed at t = 3")
			possessionVar(3).observe(true)
			val possessProbTHREE: Double = VariableElimination.probability(possessionVar(5), true)


			Then("Markov assumption doesn't apply; the probability of possession at t = 5 differs from prior " +
				"probability of possession at t = 5")
			assert(possessProbTHREE != priorProb)

			Console.println(s"\n(F1, S2) Prior probability of possession at t = 5: \t $priorProb")
			Console.println(s"(F1, S2) Probability of possession at t = 5 after observing possession at t = " +
				s"3: \t$possessProbTHREE")

		}

		Scenario("Compare possessions at t = 2, and t = 3 (two time steps behind current time t = 5)"){
			Given("no current observed ball possession at t = 5")

			When("ball is possessed at t = 2, and then also at t = 3")
			possessionVar(2).observe(true)
			val possessProbTWO: Double = VariableElimination.probability(possessionVar(5), true)

			possessionVar(3).observe(true)
			val possessProbTHREE: Double = VariableElimination.probability(possessionVar(5), true)



			Then("Markov assumption applies for those consecutive time steps; probability of possession at t = 5" +
				" after observing possession at t = 2 and t = 3 should be equal")
			assert(possessProbTWO === (possessProbTHREE +- TOLERANCE))

			Console.println(s"\n(F1, S3) Probability of possession at t = 5 after observing possession at t = " +
				s"2: \t $possessProbTWO")
			Console.println(s"(F1, S3) Probability of possession at t = 5 after observing possession at t = 2, and" +
				s" t = " +
				s"3: \t$possessProbTHREE")
		}


	}





	info("Markov Assumption Test for Immediate Past: ")
	info("Possession at current time t = 5")
	info("depends only on")
	info("possession at previous time point 4")
	info("and is independent of possessions at previous times.")


	Feature("How Markov Assumption Operates in Immediate Past: the state at any time point is conditionally " +
		"independent of all earlier states given the directly previous state") {

		Scenario("") {

			Given("observe no ball possession at t = 4")
			val possessProbPrior: Double = VariableElimination.probability(possessionVar(5), true)


			When("observe ball possession SEPERATELY at earlier times (t = 3, 2, 1 ...) other than immediately " +
				"preceding time t = 4")

			possessionVar(3).observe(false)
			val possessProbTHREE = VariableElimination.probability(possessionVar(5), true)
			possessionVar(3).unobserve()

			possessionVar(2).observe(true)
			val possessProbTWO = VariableElimination.probability(possessionVar(5), true)
			possessionVar(2).unobserve()

			possessionVar(1).observe(true)
			val possessProbONE = VariableElimination.probability(possessionVar(5), true)
			possessionVar(1).unobserve()

			possessionVar(0).observe(false)
			val possessProbZERO = VariableElimination.probability(possessionVar(5), true)
			possessionVar(0).unobserve()



			Then("the new observations don't change the prior probability of possession")

			assert(possessProbPrior === (0.48 +- TOLERANCE))


			assert(possessProbTHREE === (possessProbPrior +- TOLERANCE),
				"(F2, S1) Probability of possession at t = 5 | observed possession at t = 3")

			assert(possessProbTWO === (possessProbPrior +- TOLERANCE),
				"(F2, S1) Probability of possession at t = 5 | observed possession at t = 2, 3")

			assert(possessProbONE === (possessProbPrior +- TOLERANCE),
				"(F2, S1) Probability of possession at t = 5 | observed possession at t = 1, 2, 3")

			assert(possessProbZERO === (possessProbPrior +- TOLERANCE),
				"(F2, S1) Probability of possession at t = 5 | observed possession at t = 0, 1, 2, 3")

			Console.println(s"(F2, S1) Probability of possession at t = 5 | observe possession at t = 3: \t " +
				s"$possessProbTHREE")
			Console.println(s"(F2, S1) Probability of possession at t = 5 | observe possession at t = 2: \t " +
				s"$possessProbTWO")
			Console.println(s"(F2, S1) Probability of possession at t = 5 | observe possession at t = 1: \t " +
				s"$possessProbONE")
			Console.println(s"(F2, S1) Probability of possession at t = 5 | observe possession at t = 0: \t " +
				s"$possessProbZERO")
		}


		Scenario("Markov assumption doesn't apply: " +
			"the probabilities of possession after the new observations, given no observation at immediately " +
			"preceding time t = 4, are not different than the prior probability of possession. " +
			"So the probability " +
			"of possession at t = 5 from earlier are not independent from prior probability of possession at t = " +
			"5.") {



			Given("observe no ball possession at t = 4")
			val possessProbPrior: Double = VariableElimination.probability(possessionVar(5), true)


			When("observe ball possession at earlier times (t = 3, 2, 1 ...) other than immediately preceding time" +
				" t = 4")

			possessionVar(3).observe(true)
			val possessProbTHREE: Double = VariableElimination.probability(possessionVar(5), true)
			possessionVar(2).observe(false)
			val possessProbTWO: Double = VariableElimination.probability(possessionVar(5), true)
			possessionVar(1).observe(true)
			val possessProbONE: Double = VariableElimination.probability(possessionVar(5), true)
			possessionVar(0).observe(false)
			val possessProbZERO: Double = VariableElimination.probability(possessionVar(5), true)



			Then("the new observations don't change the prior probability of possession")

			assert(possessProbPrior === (0.48 +- TOLERANCE))


			assert(possessProbTHREE === (possessProbPrior +- TOLERANCE),
				"(F2, S1) Probability of possession at t = 5 | observed possession at t = 3")

			assert(possessProbTWO === (possessProbPrior +- TOLERANCE),
				"(F2, S1) Probability of possession at t = 5 | observed possession at t = 2, 3")

			assert(possessProbONE === (possessProbPrior +- TOLERANCE),
				"(F2, S1) Probability of possession at t = 5 | observed possession at t = 1, 2, 3")

			assert(possessProbZERO === (possessProbPrior +- TOLERANCE),
				"(F2, S1) Probability of possession at t = 5 | observed possession at t = 0, 1, 2, 3")

			Console.println(s"\n(F2, S1) Prior probability of possession at t = 5: \t $possessProbPrior")
			Console.println(s"(F2, S1) Probability of possession at t = 5 | observe possession at t = 3: \t " +
				s"$possessProbTHREE")
			Console.println(s"(F2, S1) Probability of possession at t = 5 | observe possession at t = 2, 3: \t " +
				s"$possessProbTWO")
			Console.println(s"(F2, S1) Probability of possession at t = 5 | observe possession at t = 1,2,3: \t " +
				s"$possessProbONE")
			Console.println(s"(F2, S1) Probability of possession at t = 5 | observe possession at t = 0,1,2,3: \t " +
				s"$possessProbZERO")

		}


		Scenario("Markov assumption applies: P( Poss(5) _|_ Poss(3,2,1..) | Poss(4)" +
			"the new observations don't change the probability of possession from t = 4. " +
			"Whether you had possession at t = 5 is independent of whether you had possession at earlier times" +
			"given whether you had possession at t = 4. ") {

			val possessProbPrior: Double = VariableElimination.probability(possessionVar(5), true)


			Given("observe ball possession at t = 4")
			possessionVar(4).observe(true)
			val possessProbFOUR: Double = VariableElimination.probability(possessionVar(5), true)


			When("observe ball possession at earlier times (t = 3, 2, 1 ...)")

			possessionVar(3).observe(false)
			val possessProbTHREE: Double = VariableElimination.probability(possessionVar(5), true)
			possessionVar(2).observe(true)
			val possessProbTWO: Double = VariableElimination.probability(possessionVar(5), true)
			possessionVar(1).observe(true)
			val possessProbONE: Double = VariableElimination.probability(possessionVar(5), true)
			possessionVar(0).observe(false)
			val possessProbZERO: Double = VariableElimination.probability(possessionVar(5), true)



			Then("the new observations don't change the probability of possesion from t = 4.")

			assert(possessProbPrior === (0.42874500000000004 +- TOLERANCE))

			assert(possessProbPrior != possessProbFOUR &&
				possessProbPrior != possessProbTHREE &&
				possessProbPrior != possessProbTWO &&
				possessProbPrior != possessProbONE &&
				possessProbPrior != possessProbZERO,
				"(F2, S2) Probability of possession at t = 5 changes for earlier times " +
					"once we observe possession at immediately preceding time t = 4")


			assert(possessProbFOUR === (0.6 +- TOLERANCE),
				"(F2, S2) Probability of possession at t = 5 | observed possession at t = 4")

			assert(possessProbTHREE === (0.6 +- TOLERANCE),
				"(F2, S2) Probability of possession at t = 5 | observed possession at t = 3, 4")

			assert(possessProbTWO === (0.6 +- TOLERANCE),
				"(F2, S2) Probability of possession at t = 5 | observed possession at t = 2, 3, 4")

			assert(possessProbONE === (0.6 +- TOLERANCE),
				"(F2, S2) Probability of possession at t = 5 | observed possession at t = 1, 2, 3, 4")

			assert(possessProbZERO === (0.6 +- TOLERANCE),
				"(F2, S2) Probability of possession at t = 5 | observed possession at t = 0, 1, 2, 3, 4")

		}


	}


	/*possessionVar(4).unobserve()
	possessionVar(3).unobserve()

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
	}*/
}
