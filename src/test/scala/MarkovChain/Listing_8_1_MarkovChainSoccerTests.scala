package MarkovChain



import com.cra.figaro.algorithm.factored.VariableElimination
import com.cra.figaro.language._
import org.scalatest._
import org.scalatest.featurespec.AnyFeatureSpec

import scala.collection.mutable

//import org.scalatest.matchers.should.Matchers._
//import org.scalactic.TypeCheckedTripleEquals._
import org.scalactic.Tolerance._

//import org.specs2.mutable._




object MarkovChainSoccerState {


	import Listing_8_1_MarkovChainSoccer._

	//var possessionVar: Array[Element[Boolean]] = createMarkovSoccerChain(length = 90)

	final val FIVE = 5
	final val FOUR = 4
	final val THREE: Int = 3
	final val TWO: Int = 2
	final val SIX: Int = 6
	final val SEVEN: Int = 7

	final val TOLERANCE: Double = 0.00000001


	/*/**
	 * Given a markov chain, this function resets the state given the limit or time step
	 * @param variable
	 * @param TIME_STEP_LIMIT
	 * @return
	 */
	def resetChain(variable: Array[Element[Boolean]], TIME_STEP_LIMIT: Int = 10) = {
		(0 until TIME_STEP_LIMIT).map(num => variable(num).unobserve())

		variable
	}*/

	/**
	 * Simple check to see if the given elements are all distinct or not
	 * @param elements
	 * @return
	 */
	def allDifferent(elements: Double*): Boolean = {
		List(elements).distinct.length == List(elements).length
	}
	//---------------------------------------------------------------------------------------------------------------------


	def approximatelyEqual(num1: Double, num2: Double, precision: Double): Boolean = if ( (num1 - num2).abs < precision)
		true else false


	def approxEqual(xs: Double*)(implicit precision: Double = TOLERANCE): Boolean = {
		xs.combinations(n = 2).forall{ case Seq(e1, e2) => approximatelyEqual(e1, e2, precision = precision)}
	}

	def notAllSame(xs: Double*)(implicit precision: Double = TOLERANCE): Boolean = {

		// Contains elements true or false indicating if the pairs in that location were equal (with tolerance) or not.
		val pairsEqual: List[Boolean] = xs.combinations(n = 2).map{
			case Seq(e1, e2) => approximatelyEqual(e1, e2, precision = precision )
		}.toList

		//val wasNonEqualPair: Boolean = pairsEqual.exists(isPairEqual => !isPairEqual) // does there exist at least one
		// pair that wasn't equal?

		val wereAllPairsEqual: Boolean = pairsEqual.reduceLeft(_ && _ )

		! wereAllPairsEqual
	}


	/**
	 * Checks that all elements are either never equal to one another, or there are some equal ones among them but
	 * there is at least one pair that are not equal.
	 *
	 * Never returns TRUE when all the elements are equal
	 */



	// GOAL: querying the markov model for the probability distribution over the state variable Possession at
	// any time point, given observations at any time points.


	/**
	 * 1) First ask for probability before observing any evidence:
	 */
	// Query probability that you have possession at time step 5, before observing any evidence:
	// This is also called the prior probability that you have possession at time step 5.
	/*println(VariableElimination.probability(target = possessionVar(5), value = true))

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
	println(VariableElimination.probability(possessionVar(5), true))*/

}



class Listing_8_1_MarkovChainSoccerTests extends AnyFeatureSpec with GivenWhenThen {


	import MarkovChainSoccerState._
	import Listing_8_1_MarkovChainSoccer._


	/*info("Markov Assumption Test for Non-Immediate Past: ")
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


	}*/


	info("Markov Assumption Test for Non-Immediate Past: " +
		"\nProbability of possession at current time (t = 5)" +
		"\nis independent of" +
		"\nprobability of possession at earlier non-previous times" +
		"\ngiven (no) observed possession at immediate past (t = 4)." +
		"\n(EXCEPTION: when observations of possession occur non-cumulatively, probability of possession is " +
		"dependent).")


	Feature("How Markov Assumption Operates in Non-Immediate Past: " +
		"the state of any time point is conditionally independent of all earlier states given the directly " +
		"previous state (not observed)") {



		// INDEPENDENT + NOT FOUR

		Scenario("Markov Assumption does NOT apply: " +
			"Given no observed possession at t = 4, new SEPARATE observations of possession at t = 3, 2, 1, 0 " +
			"change the probability of possession at t = 5. ") {


			val possessionVar: Array[Element[Boolean]] = createMarkovSoccerChain(length = 90)

			val possessProbPrior: Double = VariableElimination.probability(possessionVar(5), true)


			Given("observe no ball possession at t = 4")



			When("observe ball possession SEPERATELY at earlier times (t = 3, 2, 1 ...) excluding the immediately " +
				"preceding time t = 4 ...")

			possessionVar(3).observe(true)
			val possessProbTHREE = VariableElimination.probability(possessionVar(5), true)
			possessionVar(3).unobserve()

			possessionVar(2).observe(false)
			val possessProbTWO = VariableElimination.probability(possessionVar(5), true)
			possessionVar(2).unobserve()

			possessionVar(1).observe(true)
			val possessProbONE = VariableElimination.probability(possessionVar(5), true)
			possessionVar(1).unobserve()

			possessionVar(0).observe(false)
			val possessProbZERO = VariableElimination.probability(possessionVar(5), true)
			possessionVar(0).unobserve()



			Then("the new observations DO change the probability of possession at  t = 5.")

			assert(allDifferent(possessProbTHREE, possessProbTWO, possessProbONE, possessProbZERO) ||
				notAllSame(possessProbTHREE, possessProbTWO, possessProbONE, possessProbZERO),
				"Probability of possession at t = 5 must be different (or at least not all the same), for " +
					"INDIVIDUAL observations of possession")


			Console.println(s"\n(F1, S1) Prior probability of possession at t = 5: \t $possessProbPrior")
			Console.println(s"(F1, S1) Probability of possession at t = 5 | observe possession at t = 3: \t " +
				s"$possessProbTHREE")
			Console.println(s"(F1, S1) Probability of possession at t = 5 | observe possession at t = 2: \t " +
				s"$possessProbTWO")
			Console.println(s"(F1, S1) Probability of possession at t = 5 | observe possession at t = 1: \t " +
				s"$possessProbONE")
			Console.println(s"(F1, S1) Probability of possession at t = 5 | observe possession at t = 0: \t " +
				s"$possessProbZERO")
		}

		// DEPENDENT + NOT FOUR

		Scenario("Markov Assumption applies: " +
			"Given no observed possession at t = 4, new CUMULATIVE observations of possession at t = 3, 2, 1, 0 do" +
			" not change the probability of possession at t = 5. " +
			"Markov assumption implies that probability of possession at t = 5 is INDEPENDENT of probability of " +
			"possession at t = 3, 2, 1, 0, given (no) observed possession at t = 4. ") {


			val possessionVar: Array[Element[Boolean]] = createMarkovSoccerChain(length = 90)

			val possessProbPrior: Double = VariableElimination.probability(possessionVar(5), true)


			Given("observe no ball possession at t = 4")



			When("observe ball possession CUMULATIVELY at earlier times t = 3, 2, 1, 0...")

			possessionVar(3).observe(true)
			val possessProbTHREE: Double = VariableElimination.probability(possessionVar(5), true)
			possessionVar(2).observe(false)
			val possessProbTWO: Double = VariableElimination.probability(possessionVar(5), true)
			possessionVar(1).observe(true)
			val possessProbONE: Double = VariableElimination.probability(possessionVar(5), true)
			possessionVar(0).observe(false)
			val possessProbZERO: Double = VariableElimination.probability(possessionVar(5), true)



			Then("the new observations don't change the probability of possession at t = 5.")


			assert(List(possessProbTHREE,
				possessProbTWO,
				possessProbONE,
				possessProbZERO).forall(prob => prob === (0.48 +- TOLERANCE)))

			assert(approxEqual(possessProbTHREE, possessProbTWO, possessProbONE, possessProbZERO))


			Console.println(s"\n(F1, S2) Prior probability of possession at t = 5: \t $possessProbPrior")
			Console.println(s"(F1, S2) Probability of possession at t = 5 | observe possession at t = 3: \t " +
				s"$possessProbTHREE")
			Console.println(s"(F1, S2) Probability of possession at t = 5 | observe possession at t = 2,3: \t " +
				s"$possessProbTWO")
			Console.println(s"(F1, S2) Probability of possession at t = 5 | observe possession at t = 1,2,3: \t " +
				s"$possessProbONE")
			Console.println(s"(F1, S2) Probability of possession at t = 5 | observe possession at t = 0,1,2,3: \t " +
				s"$possessProbZERO")

		}
	}




	info("Markov Assumption Test for Immediate Past: " +
		"\nProbability of possession at current time (t = 5)" +
		"\nis independent of" +
		"\nprobability of possession at earlier non-previous times" +
		"\ngiven observed possession at immediate past (t = 4).")


	Feature("How Markov Assumption Operates in Immediate Past: " +
		"the state of any time point is conditionally independent of all earlier states given the directly " +
		"previous state (observed)") {


		// INDEPENDENT + FOUR
		Scenario("Markov Assumption applies: " +
			"Given observed possession at t = 4, new SEPARATE observations of possession at t = 3, 2, 1, 0 do" +
			" not change the probability of possession at t = 5. " +
			"Markov assumption implies that probability of possession at t = 5 is INDEPENDENT of probability of " +
			"possession at t = 3, 2, 1, 0, given observed possession at t = 4. ")  {


			val possessionVar: Array[Element[Boolean]] = createMarkovSoccerChain(length = 90)

			val possessProbPrior: Double = VariableElimination.probability(possessionVar(5), true)



			Given("observe ball possession at t = 4")
			possessionVar(4).observe(true)
			val possessProbFOUR: Double = VariableElimination.probability(possessionVar(5), true)



			When("observe ball possession SEPARATELY at earlier times (t = 3, 2, 1 ...) excluding the immediately" +
				"preceding time t = 4 ...")

			possessionVar(3).observe(true)
			val possessProbTHREE = VariableElimination.probability(possessionVar(5), true)
			possessionVar(3).unobserve()

			possessionVar(2).observe(false)
			val possessProbTWO = VariableElimination.probability(possessionVar(5), true)
			possessionVar(2).unobserve()

			possessionVar(1).observe(true)
			val possessProbONE = VariableElimination.probability(possessionVar(5), true)
			possessionVar(1).unobserve()

			possessionVar(0).observe(false)
			val possessProbZERO = VariableElimination.probability(possessionVar(5), true)
			possessionVar(0).unobserve()



			Then("the new observations don't change the probability of possession at t = 5.")

			assert(List(possessProbFOUR,
				possessProbTHREE,
				possessProbTWO,
				possessProbONE,
				possessProbZERO).forall(prob => prob === (0.6 +- TOLERANCE)))

			assert(
				approxEqual(possessProbFOUR, possessProbTHREE, possessProbTWO, possessProbONE, possessProbZERO)
			)


			Console.println(s"\n(F2, S1) Prior probability of possession at t = 5: \t $possessProbPrior")
			Console.println(s"(F2, S1) Probability of possession at t = 5 | observe possession at t = 4: \t " +
				s"$possessProbFOUR")
			Console.println(s"(F2, S1) Probability of possession at t = 5 | observe possession at t = 3,4: \t " +
				s"$possessProbTHREE")
			Console.println(s"(F2, S1) Probability of possession at t = 5 | observe possession at t = 2,4: \t " +
				s"$possessProbTWO")
			Console.println(s"(F2, S1) Probability of possession at t = 5 | observe possession at t = 1,4: \t " +
				s"$possessProbONE")
			Console.println(s"(F2, S1) Probability of possession at t = 5 | observe possession at t = 0,4: \t " +
				s"$possessProbZERO")
		}



		// DEPENDENT + FOUR
		Scenario("Markov Assumption applies: " +
			"Given observed possession at t = 4, new CUMULATIVE observations of possession at t = 4, 3, 2, 1, 0 do" +
			" not change the probability of possession at t = 5. " +
			"Markov assumption implies that probability of possession at t = 5 is INDEPENDENT of probability of " +
			"possession at t = 4, 3, 2, 1, 0, given observed possession at t = 4. ")  {


			val possessionVar: Array[Element[Boolean]] = createMarkovSoccerChain(length = 90)

			val possessProbPrior: Double = VariableElimination.probability(possessionVar(5), true)

			Given("observe ball possession at t = 4")
			possessionVar(4).observe(true)
			val possessProbFOUR: Double = VariableElimination.probability(possessionVar(5), true)


			When("observe ball possession CUMULATIVELY at earlier times t = 3, 2, 1, 0...")

			possessionVar(3).observe(true)
			val possessProbTHREE: Double = VariableElimination.probability(possessionVar(5), true)
			possessionVar(2).observe(false)
			val possessProbTWO: Double = VariableElimination.probability(possessionVar(5), true)
			possessionVar(1).observe(true)
			val possessProbONE: Double = VariableElimination.probability(possessionVar(5), true)
			possessionVar(0).observe(false)
			val possessProbZERO: Double = VariableElimination.probability(possessionVar(5), true)



			Then("the new observations don't change the probability of possession at t = 5.")

			assert(
				approxEqual(possessProbFOUR, possessProbTHREE, possessProbTWO, possessProbONE, possessProbZERO)
			)

			assert(List(possessProbFOUR,
				possessProbTHREE,
				possessProbTWO,
				possessProbONE,
				possessProbZERO).forall(prob => prob === (0.6 +- TOLERANCE)))


			Console.println(s"\n(F2, S2) Prior probability of possession at t = 5: \t $possessProbPrior")
			Console.println(s"(F2, S2) Probability of possession at t = 5 | observe possession at t = 4: \t " +
				s"$possessProbFOUR")
			Console.println(s"(F2, S2) Probability of possession at t = 5 | observe possession at t = 3,4: \t " +
				s"$possessProbTHREE")
			Console.println(s"(F2, S2) Probability of possession at t = 5 | observe possession at t = 2,3,4: \t " +
				s"$possessProbTWO")
			Console.println(s"(F2, S2) Probability of possession at t = 5 | observe possession at t = 1,2,3,4: \t " +
				s"$possessProbONE")
			Console.println(s"(F2, S2) Probability of possession at t = 5 | observe possession at t = 0,1,2,3,4: \t " +
				s"$possessProbZERO")
		}

	}








	info("Markov Assumption Test for Non-Immediate Future: " +
		"\nProbability of possession at current time (t = 5)" +
		"\nis independent of" +
		"\nprobability of possession at future times" +
		"\ngiven (no) observed possession at immediate future (t = 6)." +
		"\n(EXCEPTION: when observations of possession occur non-cumulatively, probability of possession is " +
		"dependent).")


	Feature("How Markov Assumption Operates in Non-Immediate Future: " +
		"the state of any time point is conditionally independent of all future states " +
		"given the next immediate state (not observed)") {

		// INDEPENDENT + NOT SIX

		Scenario("Markov Assumption does NOT apply: " +
			"Given no observed possession at t = 6, new SEPARATE observations of possession at t = 7, 8, ... " +
			"change the probability of possession at t = 5. ") {

			val possessionVar: Array[Element[Boolean]] = createMarkovSoccerChain(length = 90)

			val possessProbPrior: Double = VariableElimination.probability(possessionVar(5), true)


			Given("observe no ball possession at t = 6")



			When("observe ball possession SEPERATELY at future times (t = 7,8,...) excluding the next " +
				"immediate time t = 6 ...")

			possessionVar(7).observe(true)
			val possessProbSEVEN = VariableElimination.probability(possessionVar(5), true)
			possessionVar(7).unobserve()

			possessionVar(8).observe(false)
			val possessProbEIGHT = VariableElimination.probability(possessionVar(5), true)
			possessionVar(8).unobserve()

			possessionVar(9).observe(true)
			val possessProbNINE = VariableElimination.probability(possessionVar(5), true)
			possessionVar(9).unobserve()

			possessionVar(10).observe(false)
			val possessProbTEN = VariableElimination.probability(possessionVar(5), true)
			possessionVar(10).unobserve()



			Then("the new observations DO change the probability of possession at  t = 5.")

			assert(allDifferent(possessProbSEVEN, possessProbNINE, possessProbNINE, possessProbTEN) ||
				notAllSame(possessProbSEVEN, possessProbNINE, possessProbNINE, possessProbTEN),
				"Probability of possession at t = 5 must be different (or at least not all the same), for " +
					"INDIVIDUAL observations of possession")


			Console.println(s"\n(F3, S1) Prior probability of possession at t = 5: \t $possessProbPrior")
			Console.println(s"(F3, S1) Probability of possession at t = 5 | observe possession at t = 7: \t " +
				s"$possessProbSEVEN")
			Console.println(s"(F3, S1) Probability of possession at t = 5 | observe possession at t = 8: \t " +
				s"$possessProbNINE")
			Console.println(s"(F3, S1) Probability of possession at t = 5 | observe possession at t = 9: \t " +
				s"$possessProbNINE")
			Console.println(s"(F3, S1) Probability of possession at t = 5 | observe possession at t = 10: \t " +
				s"$possessProbTEN")
		}


		// DEPENDENT + NOT SIX

		Scenario("Markov Assumption applies: " +
			"Given no observed possession at t = 6, new CUMULATIVE observations of possession at t = 7,8,9,10 do" +
			" not change the probability of possession at t = 5. " +
			"Markov assumption implies that probability of possession at t = 5 is INDEPENDENT of probability of " +
			"possession at t = 7, 8, 9, 10, given (no) observed possession at t = 6. ") {


			val possessionVar: Array[Element[Boolean]] = createMarkovSoccerChain(length = 90)

			val possessProbPrior: Double = VariableElimination.probability(possessionVar(5), true)


			Given("observe no ball possession at t = 6 ...")



			When("observe ball possession CUMULATIVELY at future times (t = 7, 8, 9, 10 ...) excluding the next " +
				"immediate time t = 6 ...")

			possessionVar(7).observe(true)
			val possessProbSEVEN = VariableElimination.probability(possessionVar(5), true)

			possessionVar(8).observe(false)
			val possessProbEIGHT = VariableElimination.probability(possessionVar(5), true)

			possessionVar(9).observe(true)
			val possessProbNINE = VariableElimination.probability(possessionVar(5), true)

			possessionVar(10).observe(false)
			val possessProbTEN = VariableElimination.probability(possessionVar(5), true)



			Then("the new observations don't change the probability of possession at t = 5.")

			assert(
				approxEqual(possessProbSEVEN, possessProbEIGHT, possessProbNINE, possessProbTEN)
			)

			assert(List(possessProbSEVEN,
				possessProbEIGHT,
				possessProbNINE,
				possessProbTEN).forall(prob => prob === (0.4801768975520843 +- TOLERANCE)))


			Console.println(s"\n(F3, S2) Prior probability of possession at t = 5: \t $possessProbPrior")
			Console.println(s"(F3, S2) Probability of possession at t = 5 | observe possession at t = 7: \t " +
				s"$possessProbSEVEN")
			Console.println(s"(F3, S2) Probability of possession at t = 5 | observe possession at t = 7,8: \t " +
				s"$possessProbEIGHT")
			Console.println(s"(F3, S2) Probability of possession at t = 5 | observe possession at t = 7,8,9: \t " +
				s"$possessProbNINE")
			Console.println(s"(F3, S2) Probability of possession at t = 5 | observe possession at t = 7,8,9,10: \t " +
				s"$possessProbTEN")

		}
	}




	info("Markov Assumption Test for Immediate Future: " +
		"\nProbability of possession at current time (t = 5)" +
		"\nis independent of" +
		"\nprobability of possession at future (not immediately next) times" +
		"\ngiven observed possession at immediate future (t = 6).")


	Feature("How Markov Assumption Operates in Immediate Future: " +
		"the state of any time point is conditionally independent of all next states given the directly " +
		"next state (observed)") {


		// INDEPENDENT + SIX

		Scenario("Markov Assumption applies: " +
			"Given observed possession at t = 6, new SEPARATE observations of possession at t = 7, 8, 9, 10... do" +
			" not change the probability of possession at t = 5. " +
			"Markov assumption implies that probability of possession at t = 5 is INDEPENDENT of probability of " +
			"possession at t = 7, 8, 9, 10, given observed possession at t = 6. ")  {


			val possessionVar: Array[Element[Boolean]] = createMarkovSoccerChain(length = 90)

			val possessProbPrior: Double = VariableElimination.probability(possessionVar(5), true)



			Given("observe ball possession at t = 6 ...")
			possessionVar(6).observe(true)
			val possessProbSIX: Double = VariableElimination.probability(possessionVar(5), true)



			When("observe ball possession SEPARATELY at future times (t = 7,8,9,10 ...) excluding the" +
				"immediately preceding time t = 6 ...")

			possessionVar(7).observe(true)
			val possessProbSEVEN = VariableElimination.probability(possessionVar(5), true)
			possessionVar(7).unobserve()

			possessionVar(8).observe(false)
			val possessProbEIGHT = VariableElimination.probability(possessionVar(5), true)
			possessionVar(8).unobserve()

			possessionVar(9).observe(true)
			val possessProbNINE = VariableElimination.probability(possessionVar(5), true)
			possessionVar(9).unobserve()

			possessionVar(10).observe(false)
			val possessProbTEN = VariableElimination.probability(possessionVar(5), true)
			possessionVar(10).unobserve()



			Then("the new observations don't change the probability of possession at t = 5.")

			assert(List(possessProbSIX,
				possessProbSEVEN,
				possessProbEIGHT,
				possessProbNINE,
				possessProbTEN).forall(prob => prob === (0.6001700793353608 +- TOLERANCE)))

			assert(
				approxEqual(possessProbSIX, possessProbSEVEN, possessProbEIGHT, possessProbNINE, possessProbTEN)
			)


			Console.println(s"\n(F4, S1) Prior probability of possession at t = 5: \t $possessProbPrior")
			Console.println(s"(F4, S1) Probability of possession at t = 5 | observe possession at t = 6: \t " +
				s"$possessProbSIX")
			Console.println(s"(F4, S1) Probability of possession at t = 5 | observe possession at t = 6,7: \t " +
				s"$possessProbSEVEN")
			Console.println(s"(F4, S1) Probability of possession at t = 5 | observe possession at t = 6,8: \t " +
				s"$possessProbEIGHT")
			Console.println(s"(F4, S1) Probability of possession at t = 5 | observe possession at t = 6,9: \t " +
				s"$possessProbNINE")
			Console.println(s"(F4, S1) Probability of possession at t = 5 | observe possession at t = 6,10: \t " +
				s"$possessProbTEN")
		}



		// DEPENDENT + SIX

		Scenario("Markov Assumption applies: " +
			"Given observed possession at t = 6, new CUMULATIVE observations of possession at t = 7,8,9,10 do" +
			" not change the probability of possession at t = 5. " +
			"Markov assumption implies that probability of possession at t = 5 is INDEPENDENT of probability of " +
			"possession at t = 6,7,8,9,10, given observed possession at t = 6. ")  {


			val possessionVar: Array[Element[Boolean]] = createMarkovSoccerChain(length = 90)

			val possessProbPrior: Double = VariableElimination.probability(possessionVar(5), true)


			Given("observe ball possession at t = 6 ...")
			possessionVar(6).observe(true)
			val possessProbSIX = VariableElimination.probability(possessionVar(5), true)



			When("observe ball possession CUMULATIVELY at earlier times t = 7,8,9,10...")

			possessionVar(7).observe(true)
			val possessProbSEVEN = VariableElimination.probability(possessionVar(5), true)

			possessionVar(8).observe(false)
			val possessProbEIGHT = VariableElimination.probability(possessionVar(5), true)

			possessionVar(9).observe(true)
			val possessProbNINE = VariableElimination.probability(possessionVar(5), true)

			possessionVar(10).observe(false)
			val possessProbTEN = VariableElimination.probability(possessionVar(5), true)



			Then("the new observations don't change the probability of possession at t = 5.")

			assert(List(possessProbSIX,
				possessProbSEVEN,
				possessProbEIGHT,
				possessProbNINE,
				possessProbTEN).forall(prob => prob === (0.6001700793353607 +- TOLERANCE)))

			assert(
				approxEqual(possessProbSIX, possessProbSEVEN, possessProbEIGHT, possessProbNINE, possessProbTEN)
			)


			Console.println(s"\n(F4, S2) Prior probability of possession at t = 5: \t $possessProbPrior")
			Console.println(s"(F4, S2) Probability of possession at t = 5 | observe possession at t = 6: \t " +
				s"$possessProbSIX")
			Console.println(s"(F4, S2) Probability of possession at t = 5 | observe possession at t = 6,7: \t " +
				s"$possessProbSEVEN")
			Console.println(s"(F4, S2) Probability of possession at t = 5 | observe possession at t = 6,7,8: \t " +
				s"$possessProbEIGHT")
			Console.println(s"(F4, S2) Probability of possession at t = 5 | observe possession at t = 6,7,8,9: \t " +
				s"$possessProbNINE")
			Console.println(s"(F4, S2) Probability of possession at t = 5 | observe possession at t = 6,7,8,9,10: \t " +
				s"$possessProbTEN")
		}

	}


	// TODO 3 apply the past info above as property based checking (arbitrary time not just t = 5)

	//TODO 4 apply the future info above as property based checking (arbtirary time not just t = 5)


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
