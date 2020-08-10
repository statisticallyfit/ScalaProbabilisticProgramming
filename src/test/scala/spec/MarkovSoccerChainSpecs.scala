package spec

import com.cra.figaro.algorithm.factored.VariableElimination
import com.cra.figaro.language._

import org.scalatest._
import org.scalatest.featurespec.AnyFeatureSpec
import org.scalatest.GivenWhenThen
import org.scalatest.matchers.should.Matchers

// NOTE: new naming system from ShouldMatchers in version 3.2.0 of scala test = https://hyp.is/QNo6ntMLEeq2Kh8qJbyFTw/www.scalatest.org/release_notes/3.2.0

//import org.specs2.matcher.ShouldMatchers

//import org.scalactic.TypeCheckedTripleEquals._
import org.scalactic.Tolerance._


import utils._


class MarkovSoccerChainSpecs extends AnyFeatureSpec with Matchers with GivenWhenThen {


	import MarkovChain.Listing_8_1_MarkovChain._
	import utils.Tester._




	// Declaring some variables as state for below tests:

	val CURRENT_TIME: Int = 5
	val IMMEDIATE_PAST: Int = 4
	val EARLIER_PASTS: List[Int] = (0 to IMMEDIATE_PAST).toList

	

	info("Markov Assumption Test for Non-Immediate Past: ")
	info("Probability of possession at current time (t = 5)")
	info("is independent of")
	info("probability of possession at earlier non-previous times")
	info("given (no) observed possession at immediate past (t = 4).")
	info("(EXCEPTION: when observations of possession occur non-cumulatively, probability of possession is dependent).")



	Feature("How Markov Assumption Operates in Non-Immediate Past: " +
		"the state of any time point is conditionally independent of all earlier states given the directly " +
		"previous state (not observed)") {

		// INDEPENDENT + NOT FOUR

		Scenario("Markov Assumption does NOT apply: " +
			"Given no observed possession at t = 4, new SEPARATE observations of possession at t = 3, 2, 1, 0 " +
			"change the probability of possession at t = 5. ") {


			val possessionVar: Array[Element[Boolean]] = createMarkovChain(length = CHAIN_LENGTH)

			val possessProbPrior: Double = VariableElimination.probability(possessionVar(5), true)


			Given("observe no ball possession at t = 4")



			When("observe ball possession at earlier times (t = 3, 2, 1 ...) in an INDEPENDENT way ... ")

			possessionVar(2).observe(false)
			val possessProbTWO = VariableElimination.probability(possessionVar(5), true)
			possessionVar(2).unobserve()

			possessionVar(0).observe(false)
			val possessProbZERO = VariableElimination.probability(possessionVar(5), true)
			possessionVar(0).unobserve()

			possessionVar(1).observe(true)
			val possessProbONE: Double = VariableElimination.probability(possessionVar(5), true)
			possessionVar(1).unobserve()

			possessionVar(3).observe(true)
			val possessProbTHREE = VariableElimination.probability(possessionVar(5), true)
			possessionVar(3).unobserve()




			Then("the new observations may change the probability of possession at t = 5:")

			equalWithTolerance(possessProbTHREE, possessProbTWO, possessProbONE, possessProbZERO) should be(false)



			And("the prior probability of possession is not necessarily equal to any of the probability of " +
				"possession after observation:")

			notAllSameWithTolerance(possessProbPrior, possessProbTHREE) should be(true)
			notAllSameWithTolerance(possessProbPrior, possessProbTWO) should be(true)
			notAllSameWithTolerance(possessProbPrior, possessProbONE) should be(true)
			notAllSameWithTolerance(possessProbPrior, possessProbZERO) should be(true)



			And("the observed probabilities may not be equal amongst themselves:")

			(notAllSame(possessProbTHREE,
				possessProbTWO,
				possessProbONE,
				possessProbZERO) ||
				notAllSameWithTolerance(possessProbTHREE,
					possessProbTWO,
					possessProbONE,
					possessProbZERO)) should be (true)


			Logger.log("(F1, S1)")(false)(
				("Prior probability of possession at t = 5", possessProbPrior)
			)
			Logger.log("(F1, S1)")(false)(
				("Probability of possession at t = 5 | observe possession at t = 2", possessProbTWO)
			)
			Logger.log("(F1, S1)")(false)(
				("Probability of possession at t = 5 | observe possession at t = 0", possessProbZERO)
			)
			Logger.log("(F1, S1)")(false)(
				("Probability of possession at t = 5 | observe possession at t = 1", possessProbONE)
			)
			Logger.log("(F1, S1)")(false)(
				("Probability of possession at t = 5 | observe possession at t = 3", possessProbTHREE)
			)
		}


		// DEPENDENT + NOT FOUR

		Scenario("Markov Assumption applies: " +
			"Given no observed possession at t = 4, new CUMULATIVE observations of possession at t = 3, 2, 1, 0 do" +
			" not change the probability of possession at t = 5. " +
			"Markov assumption implies that probability of possession at t = 5 is INDEPENDENT of probability of " +
			"possession at t = 3, 2, 1, 0, given (no) observed possession at t = 4. ") {


			val possessionVar: Array[Element[Boolean]] = createMarkovChain(length = CHAIN_LENGTH)

			val possessProbPrior: Double = VariableElimination.probability(possessionVar(5), true)


			Given("observe no ball possession at t = 4")



			When("observe ball possession at earlier times (t = 3, 2, 1, 0) in a DEPENDENT way...")

			possessionVar(2).observe(false)
			val possessProbTWO: Double = VariableElimination.probability(possessionVar(5), true)

			possessionVar(3).observe(true)
			val possessProbTHREE: Double = VariableElimination.probability(possessionVar(5), true)

			possessionVar(0).observe(false)
			val possessProbZERO: Double = VariableElimination.probability(possessionVar(5), true)

			possessionVar(1).observe(true)
			val possessProbONE: Double = VariableElimination.probability(possessionVar(5), true)



			Then("the new observations may change the probability of possession at t = 5:")

			equalWithTolerance(possessProbTHREE, possessProbTWO, possessProbONE, possessProbZERO) should be(false)


			And("the prior probability of possession is not necessarily equal to any of the probability of " +
				"possession after observation:")

			notAllSameWithTolerance(possessProbPrior, possessProbTHREE) should be(true)
			notAllSameWithTolerance(possessProbPrior, possessProbTWO) should be(true)
			notAllSameWithTolerance(possessProbPrior, possessProbONE) should be(true)
			notAllSameWithTolerance(possessProbPrior, possessProbZERO) should be(true)


			And("the observed probabilities may not be equal amongst themselves: ")

			(notAllSame(possessProbTHREE,
				possessProbTWO,
				possessProbONE,
				possessProbZERO) ||

				notAllSameWithTolerance(possessProbTHREE,
					possessProbTWO,
					possessProbONE,
					possessProbZERO)) should be (true)


			Logger.log("(F1, S2)")(false)(
				("Prior probability of possession at t = 5", possessProbPrior)
			)
			Logger.log("(F1, S2)")(false)(
				("Probability of possession at t = 5 | observe possession at t = 2", possessProbTWO)
			)
			Logger.log("(F1, S2)")(false)(
				("Probability of possession at t = 5 | observe possession at t = 3,2", possessProbTHREE)
			)
			Logger.log("(F1, S2)")(false)(
				("Probability of possession at t = 5 | observe possession at t = 0,3,2", possessProbZERO)
			)
			Logger.log("(F1, S2)")(false)(
				("Probability of possession at t = 5 | observe possession at t = 1,0,3,2", possessProbONE)
			)


		}
	}




	info("Markov Assumption Test for Immediate Past: ")
	info("Probability of possession at current time (t = 5)")
	info("is independent of")
	info("probability of possession at earlier non-previous times")
	info("given observed possession at immediate past (t = 4).")


	Feature("How Markov Assumption Operates in Immediate Past: " +
		"the state of any time point is conditionally independent of all earlier states given the directly " +
		"previous state (observed)") {


		// INDEPENDENT + FOUR
		Scenario("Markov Assumption does apply: " +
			"Given observed possession at t = 4, new SEPARATE observations of possession at t = 3, 2, 1, 0 do" +
			" NOT change the probability of possession at t = 5. " +
			"Markov assumption implies that probability of possession at t = 5 is independent of probability " +
			"of possession at t = 3, 2, 1, 0, " +
			"when using independent observations, " +
			"given observed possession at t = 4. ")  {


			val possessionVar: Array[Element[Boolean]] = createMarkovChain(length = CHAIN_LENGTH)

			val possessProbPrior: Double = VariableElimination.probability(possessionVar(5), true)



			Given("observe ball possession at t = 4 ")

			possessionVar(4).observe(true)
			val possessProbFOUR: Double = VariableElimination.probability(possessionVar(5), true)



			When("observe ball possession at earlier times (t = 3, 2, 1 ...)  in an INDEPENDENT way...")

			possessionVar(1).observe(true)
			val possessProbONE = VariableElimination.probability(possessionVar(5), true)
			possessionVar(1).unobserve()

			possessionVar(3).observe(true)
			val possessProbTHREE = VariableElimination.probability(possessionVar(5), true)
			possessionVar(3).unobserve()

			possessionVar(0).observe(false)
			val possessProbZERO = VariableElimination.probability(possessionVar(5), true)
			possessionVar(0).unobserve()

			possessionVar(2).observe(false)
			val possessProbTWO = VariableElimination.probability(possessionVar(5), true)
			possessionVar(2).unobserve()



			Then("the new observations DO NOT change the probability of possession at t = 5:")

			equalWithTolerance(possessProbFOUR,
				possessProbTHREE,
				possessProbTWO,
				possessProbONE,
				possessProbZERO) should be(true)


			And("the prior probability of possession is not necessarily equal to any of the probability of " +
				"possession after observation:")

			notAllSameWithTolerance(possessProbPrior, possessProbFOUR) should be(true)
			notAllSameWithTolerance(possessProbPrior, possessProbTHREE) should be(true)
			notAllSameWithTolerance(possessProbPrior, possessProbTWO) should be(true)
			notAllSameWithTolerance(possessProbPrior, possessProbONE) should be (true)
			notAllSameWithTolerance(possessProbPrior, possessProbZERO) should be (true)



			Logger.log("(F2, S1)")(false)(
				("Prior probability of possession at t = 5", possessProbPrior)
			)
			Logger.log("(F2, S1)")(false)(
				("Probability of possession at t = 5 | observe possession at t = 4", possessProbFOUR)
			)
			Logger.log("(F2, S1)")(false)(
				("Probability of possession at t = 5 | observe possession at t = 1,4", possessProbONE)
			)
			Logger.log("(F2, S1)")(false)(
				("Probability of possession at t = 5 | observe possession at t = 3,4", possessProbTHREE)
			)
			Logger.log("(F2, S1)")(false)(
				("Probability of possession at t = 5 | observe possession at t = 0,4", possessProbZERO)
			)
			Logger.log("(F2, S1)")(false)(
				("Probability of possession at t = 5 | observe possession at t = 2,4", possessProbTWO)
			)
		}



		// DEPENDENT + FOUR


		Scenario(s"Markov Assumption applies: " +
			s"Given observed possession at t = $IMMEDIATE_PAST, new CUMULATIVE observations of possession at " +
			s"t = $IMMEDIATE_PAST, ${EARLIER_PASTS.mkString(", ")} " +
			s"do not change the probability of possession at t = $CURRENT_TIME. " +
			s"Markov assumption implies that probability of possession at t = $CURRENT_TIME is INDEPENDENT of " +
			s"probability of possession at t = $IMMEDIATE_PAST, ${EARLIER_PASTS.mkString(", ")}, " +
			s"when using dependent observations, " +
			s"given observed possession at t = $IMMEDIATE_PAST. ")  {


			val possessionVar: Array[Element[Boolean]] = createMarkovChain(length = CHAIN_LENGTH)

			val possessProbPrior: Double = VariableElimination.probability(possessionVar(5), true)

			Given("observe ball possession at t = 4")
			possessionVar(4).observe(true)
			val possessProbFOUR: Double = VariableElimination.probability(possessionVar(5), true)



			When("observe ball possession at earlier times (t = 3, 2, 1, 0...) in a DEPENDENT way ...")

			possessionVar(1).observe(true)
			val possessProbONE = VariableElimination.probability(possessionVar(5), true)

			possessionVar(3).observe(true)
			val possessProbTHREE = VariableElimination.probability(possessionVar(5), true)

			possessionVar(0).observe(false)
			val possessProbZERO = VariableElimination.probability(possessionVar(5), true)

			possessionVar(2).observe(false)
			val possessProbTWO = VariableElimination.probability(possessionVar(5), true)



			Then("the new observations DO NOT change the probability of possession at t = 5:")

			equalWithTolerance(possessProbFOUR,
				possessProbTHREE,
				possessProbTWO,
				possessProbONE,
				possessProbZERO) should be(true)


			And("the prior probability of possession is not necessarily equal to any of the probability of " +
				"possession after observation:")

			notAllSameWithTolerance(possessProbPrior, possessProbFOUR) should be(true)
			notAllSameWithTolerance(possessProbPrior, possessProbTHREE) should be(true)
			notAllSameWithTolerance(possessProbPrior, possessProbTWO) should be(true)
			notAllSameWithTolerance(possessProbPrior, possessProbONE) should be (true)
			notAllSameWithTolerance(possessProbPrior, possessProbZERO) should be (true)



			Logger.log("(F2, S2)")(false)(
				("Prior probability of possession at t = 5", possessProbPrior)
			)
			Logger.log("(F2, S2)")(false)(
				("Probability of possession at t = 5 | observe possession at t = 4", possessProbFOUR)
			)
			Logger.log("(F2, S2)")(false)(
				("Probability of possession at t = 5 | observe possession at t = 1,4", possessProbONE)
			)
			Logger.log("(F2, S2)")(false)(
				("Probability of possession at t = 5 | observe possession at t = 3,1,4", possessProbTHREE)
			)
			Logger.log("(F2, S2)")(false)(
				("Probability of possession at t = 5 | observe possession at t = 0,3,1,4", possessProbZERO)
			)
			Logger.log("(F2, S2)")(false)(
				("Probability of possession at t = 5 | observe possession at t = 2,0,3,1,4", possessProbTWO)
			)
		}

	}



	// TODO 2 copy template of past tests above to do the future specs.

	// TODO 3 apply the past info above as property based checking (arbitrary time not just t = 5)

	//TODO 4 apply the future info above as property based checking (arbtirary time not just t = 5)


}
