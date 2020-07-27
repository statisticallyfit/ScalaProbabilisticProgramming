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


	var counter = 0 // DEBUGGING TOOL

	// number of ways to observe evidence for possession variable. Can observe the evidence in:
	// 1) increasing + in order (e.g. 0, 1,2,3, ... CURR_TIME-1, (CURR_TIME))
	// 2) decreasing + in order (reverse) (e.g. (CURR_TIME), CURR_TIME-1, ,12,11, 12... 0)
	// 3) random order (e.g. 4, 0, 6,8, ... CURR_TIME-1, ... 2, 5, 7, 3, ...)
	final val NUM_WAYS_TO_OBSERVE = 3

	// Whether we observe that the ball is possessed at current time or not (can randomly generate this, but easier
	// this way on the testing generation from computation standpoint (?))
	final val HAVE_BALL_AT_CURR_TIME = true
	//
	final val OBS_INCR = 0 // index of listbuffer corresponding to the first way of observing evidence (increasing +
	// in
	// order)
	final val OBS_DECR = 1 // index of listbuffer corresponding to second way of observing evidence (decreasing + in
	// order)
	final val OBS_RANDOM = 2 // index of listbuffer corresponding to third way of observing evidence (random order)



	// Choose the current time step dynamically (in the specs this was t = 5)
	val genCurrentTime: Gen[DiscreteTime] = Gen.choose(0, CHAIN_LENGTH - 1).suchThat(_ > 0)

	// Decide if we observe true or false (so observing happens, but need to know if possession of ball is true
	// or false)
	val genHaveBall: Gen[Observation] = Gen.oneOf(true, false)


	// Test: exclusive time past (< currentTime - 1)

	val testExclSeparObservations = forAll(genCurrentTime, genHaveBall) {

		(currentTime: Int, haveBall: Boolean) =>

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

				possessionVar(time).observe(observation = haveBall)

				val possessProb: Probability =
					VariableElimination.probability(possessionVar(currentTime), HAVE_BALL_AT_CURR_TIME)

				listOfPossessProbs += possessProb // adding this to list

				possessionVar(time).unobserve() // doing the "separate" observation tactic
			}

			// The test, checking that all the probabilities of possession should not necessarily be all equal, as
			// they are for other test cases.
			notAllSame(listOfPossessProbs: _*)

	}



	// Test: exclusive time past (< currentTime - 1)

	val testExclCumulObservations = forAll(genCurrentTime, genHaveBall) {

		(currentTime: Int, haveBall: Boolean) =>

			Console.println(s"\nITERATION = $counter | currentTime = $currentTime | haveBall = $haveBall")
			// ---------------------


			// Create the markov chain
			// length CHAIN_LENGTH, from 0 ... CHAIN_LENGTh-1
			var possessionVar: Array[Element[Boolean]] = createMarkovSoccerChain(length = CHAIN_LENGTH)

			// Create list to store the observed probabilities of soccer ball possession at current time, after
			// observations
			// Contains list of list, where inner list is specific to how the observations happened in time.
			val probsList: List[ListBuffer[Probability]] =
				List.fill[ListBuffer[Probability]](NUM_WAYS_TO_OBSERVE)(ListBuffer())


			// Create list of points in time such that they are less than current time (so in the past) but also
			// strictly less than the current time (so 0,1,2,3 for currtime = 5)
			val exclusivePastTimes: Seq[DiscreteTime] = (0 until (currentTime - 1)) //exclusive endpoint


			// Get the prior probability of possession (must do this BEFORE doing observations in for loop below)
			val priorProb: Probability =
				VariableElimination.probability(possessionVar(currentTime), HAVE_BALL_AT_CURR_TIME)


			// Way 1 to observe evidence: increasing and in order: (time is increasing)
			for { time <- exclusivePastTimes } {

				possessionVar(time).observe(haveBall)

				val possessProb: Probability =
					VariableElimination.probability(possessionVar(currentTime),HAVE_BALL_AT_CURR_TIME)

				probsList(OBS_INCR) += possessProb
			}

			// Refresh markov chain
			possessionVar = createMarkovSoccerChain(length = CHAIN_LENGTH)

			// Way 2 to observe evidence: decreasing and in order: (time is reversed)
			for { time <- exclusivePastTimes.reverse } {

				possessionVar(time).observe(haveBall)

				val possessProb: Probability =
					VariableElimination.probability(possessionVar(currentTime),HAVE_BALL_AT_CURR_TIME)

				probsList(OBS_DECR) += possessProb
			}



			// Refresh markov chain
			possessionVar = createMarkovSoccerChain(length = CHAIN_LENGTH)

			// Way 3 to observe evidence: random order
			import scala.util.Random

			val randTimes = Random.shuffle(exclusivePastTimes)

			for { time <- randTimes } {

				possessionVar(time).observe(haveBall)

				val possessProb: Probability =
					VariableElimination.probability(possessionVar(currentTime),HAVE_BALL_AT_CURR_TIME)

				probsList(OBS_RANDOM) += possessProb
			}


			// DEBUGGING --------------------------------------------------------------------------
			Console.println(s"times = ${exclusivePastTimes.mkString(", ")}")
			Console.println(s"observed probs = ${probsList.mkString("\n")}")

			counter += 1
			// The test: asserting that not all of these probabilities in the list should be the same:

			// DEBUGGING --------------------------------------------------------------------------------

			// Test 1: check the prior prob is not the same with EACH of the other possession probs
			val result1 = probsList.forall(list => list.forall(obsProb => notAllSame(obsProb, priorProb)))


			// Test 2:assert that all the observed probs are approximately equal
			val result2 = probsList.forall(list => approxEqual(list:_*))


			// DEBUGGING --------------------------------------------------------------------------------

			Console.println(s"ITERATION = ${counter - 1} \n\t| currentTime = $currentTime \n\t| haveBall = $haveBall " +
				s"\n\t| notSameEach = ${result1} \n\t| approxEqualObservedProbs = $result2")


			result1 && result2

	}

}


object Checker extends Properties("MarkovAssumption") {

	import MarkovChainSoccerProps._

	//propExclusiveSeparatePossessions.check()
	//propExclusiveCumulativePossessions.check()
	// Test: inclusive / immediate time past (<= currentTime - 1)

	// Choose the number above (future) the current time in a dynamic way ...
	// ... for non-immediate future (> currentTime + 1)
	// ... for immediate future  (>= currentTime + 1)
}




object TEMP_VerifyMarkovHoldsInAnyOrderOfObservation { //extends Properties("MarkovAssumption") {

	/**
	 * Verifying that markov assumption (in one of the cases where it applies like below is F2, S2 in specs file)
	 * holds even when switching up the order of the observed probabilities.
	 */

	import MarkovChainSoccerProps._


	var counter = 0
	val currentTime = 15
	val haveBall = true
	CHAIN_LENGTH = 20



	def autoTest = {

		println("\nTesting for loop")


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
		val exclusivePastTimes: Seq[DiscreteTime] = (0 until (currentTime - 1)).reverse //inclusive endpoint


		// Get the prior probability of possession (must do this BEFORE doing observations in for loop below)
		val priorProb: Probability = VariableElimination.probability(possessionVar(currentTime), true)


		//exclusivePastTimePoints.foreach{ time =>
		for { time <- exclusivePastTimes } {

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

		println("\n Testing manually 1")

		val possessionVar: Array[Element[Boolean]] = createMarkovSoccerChain(length = CHAIN_LENGTH)

		possessionVar(3).observe(haveBall)
		val possessProbTHREE: Double = VariableElimination.probability(possessionVar(currentTime), true)
		possessionVar(2).observe(haveBall)
		val possessProbTWO: Double = VariableElimination.probability(possessionVar(currentTime), true)
		possessionVar(1).observe(haveBall)
		val possessProbONE: Double = VariableElimination.probability(possessionVar(currentTime), true)
		possessionVar(0).observe(haveBall)
		val possessProbZERO: Double = VariableElimination.probability(possessionVar(currentTime), true)


		println(possessProbTHREE, possessProbTWO, possessProbONE, possessProbZERO)
		println("approx equal = " + approxEqual(possessProbTHREE, possessProbTWO, possessProbONE, possessProbZERO))

		// ----------------------------
		println("\n Testing manually 2")

		val possessionVar2: Array[Element[Boolean]] = createMarkovSoccerChain(length = CHAIN_LENGTH)

		possessionVar2(1).observe(haveBall)
		val possessProbONE2: Double = VariableElimination.probability(possessionVar2(currentTime), true)

		possessionVar2(3).observe(haveBall)
		val possessProbTHREE2: Double = VariableElimination.probability(possessionVar2(currentTime), true)
		possessionVar2(0).observe(haveBall)
		val possessProbZERO2: Double = VariableElimination.probability(possessionVar2(currentTime), true)

		possessionVar2(2).observe(haveBall)
		val possessProbTWO2: Double = VariableElimination.probability(possessionVar2(currentTime), true)


		println(possessProbTHREE2, possessProbTWO2, possessProbONE2, possessProbZERO2)
		println("approx equal " + approxEqual(possessProbTHREE2, possessProbTWO2, possessProbONE2, possessProbZERO2))


		// ----------------------------
		println("\n Testing manually 3")

		val possessionVar3: Array[Element[Boolean]] = createMarkovSoccerChain(length = CHAIN_LENGTH)

		possessionVar3(0).observe(haveBall)
		val possessProbZERO3: Double = VariableElimination.probability(possessionVar3(currentTime), true)
		possessionVar3(1).observe(haveBall)
		val possessProbONE3: Double = VariableElimination.probability(possessionVar3(currentTime), true)
		possessionVar3(2).observe(haveBall)
		val possessProbTWO3: Double = VariableElimination.probability(possessionVar3(currentTime), true)
		possessionVar3(3).observe(haveBall)
		val possessProbTHREE3: Double = VariableElimination.probability(possessionVar3(currentTime), true)

		println(possessProbTHREE3, possessProbTWO3, possessProbONE3, possessProbZERO3)
		println("approx equal " + approxEqual(possessProbTHREE3, possessProbTWO3, possessProbONE3, possessProbZERO3))


		// ---------------------------------------------------

		println("\n Testing manually 4")

		val possessionVar4: Array[Element[Boolean]] = createMarkovSoccerChain(length = CHAIN_LENGTH)

		possessionVar4(3).observe(false)
		val possessProbTHREE4: Double = VariableElimination.probability(possessionVar4(currentTime), true)
		possessionVar4(2).observe(true)
		val possessProbTWO4: Double = VariableElimination.probability(possessionVar4(currentTime), true)
		possessionVar4(1).observe(true)
		val possessProbONE4: Double = VariableElimination.probability(possessionVar4(currentTime), true)
		possessionVar4(0).observe(false)
		val possessProbZERO4: Double = VariableElimination.probability(possessionVar4(currentTime), true)


		println(possessProbTHREE4, possessProbTWO4, possessProbONE4, possessProbZERO4)
		println("approx equal = " + approxEqual(possessProbTHREE4, possessProbTWO4, possessProbONE4,
			possessProbZERO4))


		// ----------------------------
		println("\n Testing manually 5")

		val possessionVar5: Array[Element[Boolean]] = createMarkovSoccerChain(length = CHAIN_LENGTH)

		possessionVar5(1).observe(false)
		val possessProbONE5: Double = VariableElimination.probability(possessionVar5(currentTime), true)

		possessionVar5(3).observe(true)
		val possessProbTHREE5: Double = VariableElimination.probability(possessionVar5(currentTime), true)
		possessionVar5(0).observe(true)
		val possessProbZERO5: Double = VariableElimination.probability(possessionVar5(currentTime), true)

		possessionVar5(2).observe(false)
		val possessProbTWO5: Double = VariableElimination.probability(possessionVar5(currentTime), true)


		println(possessProbTHREE5, possessProbTWO5, possessProbONE5, possessProbZERO5)
		println("approx equal " + approxEqual(possessProbTHREE5, possessProbTWO5, possessProbONE5, possessProbZERO5))


		// ----------------------------
		println("\n Testing manually 6")

		val possessionVar6: Array[Element[Boolean]] = createMarkovSoccerChain(length = CHAIN_LENGTH)

		possessionVar6(0).observe(false)
		val possessProbZERO6: Double = VariableElimination.probability(possessionVar6(currentTime), true)
		possessionVar6(1).observe(true)
		val possessProbONE6: Double = VariableElimination.probability(possessionVar6(currentTime), true)
		possessionVar6(2).observe(true)
		val possessProbTWO6: Double = VariableElimination.probability(possessionVar6(currentTime), true)
		possessionVar6(3).observe(false)
		val possessProbTHREE6: Double = VariableElimination.probability(possessionVar6(currentTime), true)

		println(possessProbTHREE6, possessProbTWO6, possessProbONE6, possessProbZERO6)
		println("approx equal " + approxEqual(possessProbTHREE6, possessProbTWO6, possessProbONE6, possessProbZERO6))

	}


	def main(args: Array[String]) {
		autoTest
		println
		manualTest
	}
}
