package prop

/**
 *
 */



import org.scalacheck._
import Prop.{forAll, propBoolean}
import Gen._
import Arbitrary.arbitrary


import MarkovChain.Listing_8_1_MarkovChainSoccer._
import utils.Utils._


import com.cra.figaro.algorithm.factored.VariableElimination
import com.cra.figaro.language.Element

import scala.collection.mutable.ListBuffer



object MarkovChainSoccerProps {


	type DiscreteTime = Int
	type Observation = Boolean
	type Probability = Double


	CHAIN_LENGTH = 20


	// Choose the current time step dynamically (in the specs this was t = 5)
	val genCurrentTime: Gen[DiscreteTime] = Gen.choose(0, CHAIN_LENGTH - 1).suchThat(_ > 0)

	// Decide if we observe true or false (so observing happens, but need to know if possession of ball is true
	// or false)
	val genHaveBall: Gen[Observation] = Gen.oneOf(true, false)


	// Test: exclusive time past (< currentTime - 1)

	val propExclusiveSeparatePossessions = forAll(genCurrentTime, genHaveBall) {

		(currentTime: Int, yesOrNo: Boolean) =>

			// Create the markov chain
			// length CHAIN_LENGTH, from 0 ... CHAIN_LENGTh-1
			val possessionVar: Array[Element[Boolean]] = createMarkovSoccerChain(length = CHAIN_LENGTH)

			// Create list to store the observed probabilities of soccer ball possession at current time, after
			// observations
			val listOfPossessProbs: ListBuffer[Probability] = ListBuffer()

			// Create list of points in time such that they are less than current time (so in the past) but also
			// strictly less than the current time (so 0,1,2,3 for currtime = 5)
			val exclusivePastTimes: Seq[DiscreteTime] = (0 to (currentTime - 1)) //inclusive endpoint


			//exclusivePastTimePoints.foreach{ time =>
			for (time <- exclusivePastTimes) {

				possessionVar(time).observe(observation = yesOrNo)

				val possessProb: Probability = VariableElimination.probability(possessionVar(currentTime), true)

				listOfPossessProbs += possessProb // adding this to list

				possessionVar(time).unobserve() // doing the "separate" observation tactic
			}

			// The test, checking that all the probabilities of possession should not necessarily be all equal, as
			// they are for other test cases.
			notAllSame(listOfPossessProbs: _*)

	}


	var counter = 0
	// Test: exclusive time past (< currentTime - 1)

	val propExclusiveCumulativePossessions = forAll(genCurrentTime, genHaveBall) {

		(currentTime: Int, haveBall: Boolean) =>

			Console.println(s"\nITERATION = $counter | currentTime = $currentTime | haveBall = $haveBall")
			// ---------------------


			// Create the markov chain
			// length CHAIN_LENGTH, from 0 ... CHAIN_LENGTh-1
			val possessionVar: Array[Element[Boolean]] = createMarkovSoccerChain(length = CHAIN_LENGTH)

			// Create list to store the observed probabilities of soccer ball possession at current time, after
			// observations
			val listOfPossessProbs: ListBuffer[Probability] = ListBuffer()

			// Create list of points in time such that they are less than current time (so in the past) but also
			// strictly less than the current time (so 0,1,2,3 for currtime = 5)
			val exclusivePastTimes: Seq[DiscreteTime] = (0 to (currentTime - 1)) //inclusive endpoint


			// Get the prior probability of possession (must do this BEFORE doing observations in for loop below)
			val priorProb: Probability = VariableElimination.probability(possessionVar(currentTime), true)


			//exclusivePastTimePoints.foreach{ time =>
			for (time <- exclusivePastTimes) {

				possessionVar(time).observe(observation = haveBall)

				val possessProb: Probability = VariableElimination.probability(possessionVar(currentTime), true)

				listOfPossessProbs += possessProb // adding this to list

				//possessionVar(time).unobserve() // doing the "separate" observation tactic
			}

			// DEBUGGING --------------------------------------------------------------------------
			Console.println(s"times = ${exclusivePastTimes.mkString(", ")}")
			Console.println(s"observed probs = ${listOfPossessProbs.mkString("\n")}")

			counter += 1
			// The test: asserting that not all of these probabilities in the list should be the same:

			// DEBUGGING --------------------------------------------------------------------------------

			// Test 1: check the prior prob is not the same with EACH of the other possession probs
			val result1 = listOfPossessProbs.forall(obsProb => notAllSame(obsProb, priorProb))


			// Test 2:assert that all the observed probs are approximately equal
			val result2 = approxEqual(listOfPossessProbs:_*)


			// DEBUGGING --------------------------------------------------------------------------------

			Console.println(s"ITERATION = ${counter-1} \n\t| currentTime = $currentTime \n\t| haveBall = $haveBall " +
				s"\n\t| notSameEach = ${result1} \n\t| approxEqualObservedProbs = $result2")


			result1 && result2

	}

}

object Checker { //extends Properties("MarkovAssumption") {


	import MarkovChainSoccerProps._

	//propExclusiveSeparatePossessions.check()
	//propExclusiveCumulativePossessions.check()

	var counter = 0
	val currentTime = 15
	val haveBall = false
	CHAIN_LENGTH = 20



	def autoTest = {

		Console.println(s"\nITERATION = $counter | currentTime = $currentTime | haveBall = $haveBall")
		// ---------------------


		// Create the markov chain
		// length CHAIN_LENGTH, from 0 ... CHAIN_LENGTh-1
		val possessionVar: Array[Element[Boolean]] = createMarkovSoccerChain(length = CHAIN_LENGTH)

		// Create list to store the observed probabilities of soccer ball possession at current time, after
		// observations
		val listOfPossessProbs: ListBuffer[Probability] = ListBuffer()

		// Create list of points in time such that they are less than current time (so in the past) but also
		// strictly less than the current time (so 0,1,2,3 for currtime = 5)
		val exclusivePastTimes: Seq[DiscreteTime] = (0 to (currentTime - 1)) //inclusive endpoint


		// Get the prior probability of possession (must do this BEFORE doing observations in for loop below)
		val priorProb: Probability = VariableElimination.probability(possessionVar(currentTime), true)


		//exclusivePastTimePoints.foreach{ time =>
		for (time <- exclusivePastTimes) {

			possessionVar(time).observe(observation = haveBall)

			val possessProb: Probability = VariableElimination.probability(possessionVar(currentTime), true)

			listOfPossessProbs += possessProb // adding this to list

			//possessionVar(time).unobserve() // doing the "separate" observation tactic
		}

		// DEBUGGING --------------------------------------------------------------------------
		Console.println(s"times = ${exclusivePastTimes.mkString(", ")}")
		Console.println(s"observed probs = ${listOfPossessProbs.mkString("\n")}")

		counter += 1
		// The test: asserting that not all of these probabilities in the list should be the same:

		// DEBUGGING --------------------------------------------------------------------------------

		// Test 1: check the prior prob is not the same with EACH of the other possession probs
		val result1 = listOfPossessProbs.forall(obsProb => notAllSame(obsProb, priorProb))


		// Test 2:assert that all the observed probs are approximately equal
		val result2 = approxEqual(listOfPossessProbs: _*)


		// DEBUGGING --------------------------------------------------------------------------------

		Console.println(s"ITERATION = ${counter - 1} \n\t| currentTime = $currentTime \n\t| haveBall = $haveBall " +
			s"\n\t| notSameEach = ${result1} \n\t| approxEqualObservedProbs = $result2")

	}


	def manualTest = {

		println("\n Testing manually")

		val possessionVar: Array[Element[Boolean]] = createMarkovSoccerChain(length = CHAIN_LENGTH)

		val possessProbPrior: Double = VariableElimination.probability(possessionVar(5), true)



		possessionVar(3).observe(true)
		val possessProbTHREE: Double = VariableElimination.probability(possessionVar(5), true)
		possessionVar(2).observe(false)
		val possessProbTWO: Double = VariableElimination.probability(possessionVar(5), true)
		possessionVar(1).observe(true)
		val possessProbONE: Double = VariableElimination.probability(possessionVar(5), true)
		possessionVar(0).observe(false)
		val possessProbZERO: Double = VariableElimination.probability(possessionVar(5), true)


		println(possessProbTHREE, possessProbTWO, possessProbONE, possessProbZERO)
		Console.println("notAllSame(possessProbPrior, possessProbTHREE) = " + notAllSame(possessProbPrior,
			possessProbTHREE))
		println(s"notAllSame(possessProbPrior, possessProbTWO) = ${notAllSame(possessProbPrior, possessProbTWO)}")
		println(s"notAllSame(possessProbPrior, possessProbONE) = ${notAllSame(possessProbPrior, possessProbONE)}")
		println(s"notAllSame(possessProbPrior, possessProbZERO) = ${notAllSame(possessProbPrior, possessProbZERO)}")


		println("approx equal = " + approxEqual(possessProbTHREE, possessProbTWO, possessProbONE, possessProbZERO))
	}


	def main(args: Array[String]) {
		manualTest
	}
	// Test: inclusive / immediate time past (<= currentTime - 1)

	// Choose the number above (future) the current time in a dynamic way ...
	// ... for non-immediate future (> currentTime + 1)
	// ... for immediate future  (>= currentTime + 1)
}
