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


	Feature("How Markov Assumption Applies for Immediate Past: the state at any time point is conditionally " +
		"independent of all earlier states given the directly previous state") {



		Scenario("Observe possession at t = 3 (two steps behind current time t = 5)"){
			Given("no current observed ball possession at t = 5")
			val priorProb: Double = VariableElimination.probability(possessionVar(5), true)


			When("ball is possessed at t = 3")
			possessionVar(3).observe(true)
			val possessProbTHREE: Double = VariableElimination.probability(possessionVar(5), true)


			Then("Markov assumption doesn't apply; the probability of possession at t = 5 (after observing " +
				"possession at t = 3) differs from prior probability of possession at t = 5")
			//assert(priorProb != possessProbTHREE)
			Console.println(s"\n(F2, S1) Prior probability of possession at t = 5: \t $priorProb")
			Console.println(s"(F2, S1) Probability of possession at t = 5 after observing possession at t = 3: \t " +
				s"$possessProbTHREE")
		}

		Scenario("Observe possession at t = 4 (two steps behind current time t = 5)"){
			Given("no current observed ball possession at t = 5")
			val priorProb: Double = VariableElimination.probability(possessionVar(5), true)


			When("ball is possessed at t = 4")
			possessionVar(4).observe(true)
			val possessProbFOUR: Double = VariableElimination.probability(possessionVar(5), true)


			Then("Markov assumption doesn't apply; the probability of possession at t = 5 (after observing " +
				"possession at t = 4) differs from prior probability of possession at t = 5")
			//assert(priorProb != possessProbFOUR)
			Console.println(s"\n(F2, S2) Prior probability of possession at t = 5: \t $priorProb")
			Console.println(s"(F2, S2) Probability of possession at t = 5 after observing possession at t = 4: \t " +
				s"$possessProbFOUR")
		}


		Scenario("Compare possessions at t = 5 after observing possession at t = 3, 4 (immediately behind current " +
			"time t = 5)"){

			Given("no current observed ball possession at t = 5")

			When("ball is possessed at t = 4 and then t = 3")
			possessionVar(4).observe(true)
			val possessProbFOUR: Double = VariableElimination.probability(possessionVar(5), true)
			possessionVar(3).observe(true)
			val possessProbTHREE: Double = VariableElimination.probability(possessionVar(5), true)


			Then("Markov assumption applies; the probability of possession at t = 5 equals prior " +
				"probability of possession at t = 5")
			//assert(possessProbTHREE === (possessProbFOUR +- TOLERANCE))

			Console.println(s"\n(F2, S3) Probability of possession at t = 5 after observing possession at t = " +
				s"3: \t $possessProbTHREE")
			Console.println(s"(F2, S3) Probability of possession at t = 5 after observing possession at t = 3, " +
				s"4: \t $possessProbFOUR")
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
