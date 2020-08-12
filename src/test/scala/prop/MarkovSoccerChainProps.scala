package prop



import MarkovChain.Listing_8_1_MarkovChain._

import com.cra.figaro.algorithm.factored.VariableElimination
import com.cra.figaro.language.Element


import org.scalacheck._
import Prop.{forAll, propBoolean}
import Gen._
import Arbitrary.arbitrary


import org.specs2.control.Debug

import utils._
import utils.Tester._



import scala.collection.mutable.ListBuffer
import scala.util.Random

import org.scalatest.featurespec.AnyFeatureSpec
import org.scalatest.GivenWhenThen
import org.scalatest.matchers.should.Matchers

/**
 *
 */
//RULES of specs2's pp function:

// 1) if the result of the value being tested as "value.pp" is FALSE then
// eventually the tests stop running, and throw an exception. Stacks do not overlap, so no worries
// about tests running into each other.
// 2) my rule: use .pp for counter printing and use Logger for grouping several statements and for the
// DSL name "logging", since .pp is mostly specs2 testing
// 3) use .pp to scatter print statements inside the property WHEN DEBUGGING not for LOGGING (so my
//  logging DSL remains and I can additionally use .pp simultaneously in CHECK the verity of a certain
//  statement at a particular POINT.


object MarkovSoccerChainProps extends AnyFeatureSpec with GivenWhenThen with Matchers with Debug {


	type DiscreteTime = Int
	type Observation = Boolean
	type Probability = Double


	CHAIN_LENGTH = 20


	var counter = 0 // DEBUGGING TOOL
	def resetCounter(): Unit = counter = 0


	// Whether we observe that the ball is possessed at current time or not (can randomly generate this, but easier
	// this way on the testing generation from computation standpoint (?))
	final val HAVE_BALL_AT_CURR_TIME = true


	// Choose the current time step dynamically (in the specs this was t = 5)
	val genCurrentTime: Gen[DiscreteTime] = Gen.choose(0, CHAIN_LENGTH - 1).suchThat(_ > 0)

	// Decide if we observe true or false (so observing happens, but need to know if possession of ball is true
	// or false)
	val genHaveBall: Gen[Observation] = Gen.oneOf(true, false)


	// Test: exclusive time past (< currentTime - 1)
	val testEXClusiveDEPendentObservationsDoNotObeyMarkovAssumption = forAll(genCurrentTime, genHaveBall) {

		(currentTime: Int, haveBall: Boolean) =>

			Logger.log()()(("\n---------------- ITERATION", counter))


			// Declaring some variables as state for below tests:

			//val CURRENT_TIME: Int = 5
			val immediatePast: Int = currentTime - 1
			val earlierPasts: List[Int] = (0 to (immediatePast - 1)).toList

			// Create list of points in time such that they are less than current time (so in the past) but also
			// strictly less than the current time (so 0,1,2,3 for currtime = 5)
			val exclusivePastTimes: Seq[DiscreteTime] = earlierPasts

			// Shuffle the times so we can observe the ball posession at random order of times.
			val shuffledTimes = Random.shuffle(exclusivePastTimes)


			// Creating a way to display the accumulated times, based on dependencies from the for loop:
			val accumulatedTimes: Seq[Seq[DiscreteTime]] =
				shuffledTimes
					.scanLeft(Seq[DiscreteTime]())((accList, newTime) => accList :+ newTime)
					.tail

			// Create list to store the observed probabilities of soccer ball possession at current time, after
			// observations
			val probs: ListBuffer[Probability] = ListBuffer()




			Given(s"a Markov chain with length $CHAIN_LENGTH: ")

			// Create the markov chain
			// length CHAIN_LENGTH, from 0 ... CHAIN_LENGTh-1
			val possessionVar: Array[Element[Boolean]] = createMarkovChain(length = CHAIN_LENGTH)


			// Get the prior probability of possession (must do this BEFORE doing observations in for loop below)
			val priorProb: Probability =
				VariableElimination.probability(possessionVar(currentTime), HAVE_BALL_AT_CURR_TIME)




			When(s"observe no ball possession at $immediatePast ")

			exclusivePastTimes should (
				contain(earlierPasts) and (not contain(immediatePast))
				)

			shuffledTimes should (
				contain(earlierPasts) and (not contain(immediatePast))
				)


			And(s"... observe ball possession at earlier times (t = ${earlierPasts.mkString(",")} ...) in a " +
				"DEPENDENT way ... ")

			for { time <- shuffledTimes } {

				possessionVar(time).observe(haveBall)

				val possessProb: Probability = VariableElimination.probability(
					target = possessionVar(currentTime),
					value = HAVE_BALL_AT_CURR_TIME
				)


				probs += possessProb

			}



			// NOTE: dependency aspect of observations
			val accTimeProbPairs: Seq[ (Seq[DiscreteTime], Probability) ] = accumulatedTimes.zip(probs)


			// TESTING

			Then(s"the new observations may change the probability of possession at t = $currentTime:")

			// Comparing the last time in the list of the accumulated times, in each times-prob-pair, since the
			// last of the accumulated times is the current one represented in the for loop.
			val nonMarkovTimeProbs: Seq[(Seq[DiscreteTime], Probability)] =
				accTimeProbPairs
					.takeWhile{case (accumTimes, prob) => accumTimes.last != immediatePast}

			val markovTimeProbs: Seq[(Seq[DiscreteTime], Probability)] =
				accTimeProbPairs
					.dropWhile { case (accumTimes, prob) => accumTimes.last != immediatePast }

			val nonMarkovProbs: Seq[Probability] = nonMarkovTimeProbs.map(_._2)
			val markovProbs: Seq[Probability] = markovTimeProbs.map(_._2)


			// Test all observations made before current time will be different (or at least not all are the same)
			val areNonMarkovsDifferent: Boolean =
				notAllSame(nonMarkovProbs:_*) || notAllSameWithTolerance(nonMarkovProbs:_*)

			areNonMarkovsDifferent should be (true)

			equalWithTolerance(nonMarkovProbs:_*) should be (false)



			And("the prior probability of possession is not necessarily equal to any of the probability of " +
				"possession after observation:")

			val arePriorAndObservedProbsPairwiseDifferent: Boolean =
				probs.forall(obsProb => notAllSame(obsProb, priorProb) || notAllSameWithTolerance(obsProb, priorProb))

			arePriorAndObservedProbsPairwiseDifferent should be (true)




			And("the observed probabilities are never Markov (all are non-markov): ")

			markovProbs should have length(0)
			nonMarkovProbs should have length(exclusivePastTimes.length)

			//TODO how to avoid this repeating from should matchers?
			val allObsDisobeyMarkovAssumption: Boolean =
				markovProbs.isEmpty &&
					nonMarkovProbs.length == exclusivePastTimes.length




			// LOGGING


			// Logging where the non-markov probabilities are (before observing at immediate past)
			nonMarkovTimeProbs.foreach{
				case (accumTimes, prob) =>
					Logger.log()(false)((s"Probability of possession at t = $currentTime " +
						s"| observe possession at t = ${accumTimes
							.map( t => if (t == immediatePast) s"($t)" else s"$t" )
							.mkString(",")}", prob) )
			}

			// Logging the results of the tests
			Logger.log()()(
				("timeProbPairs", accTimeProbPairs.map { case(accTimes, prob) => (accTimes.last, prob)}),
				("areNonMarkovsDifferent", areNonMarkovsDifferent),
				("allObsDisobeyMarkovAssumption", allObsDisobeyMarkovAssumption)
			)

			counter += 1


			arePriorAndObservedProbsPairwiseDifferent &&
				areNonMarkovsDifferent &&
				allObsDisobeyMarkovAssumption
	}




	// Test: exclusive time past (< currentTime - 1)
	val testEXClusiveINDependentObservationsDoNotObeyMarkovAssumption = forAll(genCurrentTime, genHaveBall) {

		(currentTime: Int, haveBall: Boolean) =>


			// Declaring some variables as state for below tests:

			//val CURRENT_TIME: Int = 5
			val immediatePast: Int = currentTime - 1
			val earlierPasts: List[Int] = (0 to (immediatePast - 1)).toList



			Logger.log()()(("\n---------------- ITERATION", counter))


			// Create the markov chain
			// length CHAIN_LENGTH, from 0 ... CHAIN_LENGTh-1
			val possessionVar: Array[Element[Boolean]] = createMarkovChain(length = CHAIN_LENGTH)

			// Create list to store the observed probabilities of soccer ball possession at current time, after
			// observations
			// Contains list of list, where inner list is specific to how the observations happened in time.
			//val probs: List[ListBuffer[Probability]] = List.fill[ListBuffer[Probability]](NUM_WAYS_TO_OBSERVE)(ListBuffer())
			val probs: ListBuffer[Probability] = ListBuffer()


			// Create list of points in time such that they are less than current time (so in the past) but also
			// strictly less than the current time (so 0,1,2,3 for currtime = 5)
			val exclusivePastTimes: Seq[DiscreteTime] = earlierPasts //:+ immediatePast


			// Get the prior probability of possession (must do this BEFORE doing observations in for loop below)
			val priorProb: Probability =
				VariableElimination.probability(possessionVar(currentTime), HAVE_BALL_AT_CURR_TIME)


			// Shuffle the times so we can observe the ball posession at random order of times.
			val shuffledTimes = Random.shuffle(exclusivePastTimes)

			for { time <- shuffledTimes } {

				possessionVar(time).observe(haveBall)

				val possessProb: Probability =
					VariableElimination.probability(possessionVar(currentTime), HAVE_BALL_AT_CURR_TIME)


				// Keeping all observations INDEPENDENT, while keeping the dependency of immediate past, when it
				// appears. Effect is just to "unobserve" the times that are not the immediate past.
				// NOTE: this won't run here because times are exclusive anyway (don't contain immediate past)
				if(time != immediatePast){
					possessionVar(time).unobserve() // the separate / non-cumulative aspect
				}

				probs += possessProb

			}

			val timeProbPairs: List[(DiscreteTime, Probability)] = shuffledTimes.zip(probs).toList


			// TESTING

			// Testing 1: check the prior prob is not the same with EACH of the other possession probs
			val arePriorAndObservedProbsPairwiseDifferent: Boolean =
				probs.forall(obsProb => notAllSame(obsProb, priorProb) || notAllSameWithTolerance(obsProb, priorProb))


			// Testing 2: assert that all the observations made before currentime will be different (not all same)
			val nonMarkovTimeProbs: List[(DiscreteTime, Probability)] =
				timeProbPairs
					.takeWhile{case (time, prob) => time != immediatePast}


			val markovTimeProbs: List[(DiscreteTime, Probability)] =
				timeProbPairs
					.dropWhile { case (time, prob) => time != immediatePast } // next after dropped was observed at
			// immediate past

			val nonMarkovProbs: List[Probability] = nonMarkovTimeProbs.map(_._2)
			val markovProbs: List[Probability] = markovTimeProbs.map(_._2)


			val areNonMarkovsDifferent: Boolean =
				notAllSame(nonMarkovProbs:_*) || notAllSameWithTolerance(nonMarkovProbs:_*)


			// Testing 3: there are no markov probabilities
			val allObsDisobeyMarkovAssumption: Boolean =
				markovProbs.isEmpty &&
					nonMarkovProbs.length == exclusivePastTimes.length




			// LOGGING


			// Logging where the non-markov probabilities are (before observing at immediate past)
			nonMarkovTimeProbs.foreach{
				case (time, prob) =>
					Logger.log()(false)((s"Probability of possession at t = $currentTime " +
						s"| observe possession at t = $time", prob) )
			}

			// Logging the results of the tests
			Logger.log()()(
				("timeProbPairs", timeProbPairs ),
				("areNonMarkovsDifferent", areNonMarkovsDifferent),
				("allObsDisobeyMarkovAssumption", allObsDisobeyMarkovAssumption)
			)

			counter += 1


			arePriorAndObservedProbsPairwiseDifferent &&
				areNonMarkovsDifferent &&
				allObsDisobeyMarkovAssumption
	}




	// Test: inclusive / immediate time past (<= currentTime - 1)

	val testINClusiveINDependentObservationsObeyMarkovAssumption = forAll(genCurrentTime, genHaveBall) {

		(currentTime: Int, haveBall: Boolean) =>


			// Declaring some variables as state for below tests:

			//val CURRENT_TIME: Int = 5
			val immediatePast: Int = currentTime - 1
			val earlierPasts: List[Int] = (0 to (immediatePast - 1)).toList



			Logger.log()()(("\n---------------- ITERATION", counter))


			// Create the markov chain
			// length CHAIN_LENGTH, from 0 ... CHAIN_LENGTh-1
			val possessionVar: Array[Element[Boolean]] = createMarkovChain(length = CHAIN_LENGTH)

			// Create list to store the observed probabilities of soccer ball possession at current time, after
			// observations
			// Contains list of list, where inner list is specific to how the observations happened in time.
			//val probs: List[ListBuffer[Probability]] = List.fill[ListBuffer[Probability]](NUM_WAYS_TO_OBSERVE)(ListBuffer())
			val probs: ListBuffer[Probability] = ListBuffer()


			// Create list of points in time such that they are less than current time (so in the past) but also
			// strictly less than the current time (so 0,1,2,3 for currtime = 5)
			val inclusivePastTimes: Seq[DiscreteTime] = earlierPasts :+ immediatePast


			// Get the prior probability of possession (must do this BEFORE doing observations in for loop below)
			val priorProb: Probability =
				VariableElimination.probability(possessionVar(currentTime), HAVE_BALL_AT_CURR_TIME)


			// Shuffle the times so we can observe the ball posession at random order of times.
			val shuffledTimes = Random.shuffle(inclusivePastTimes)

			for { time <- shuffledTimes } {

				possessionVar(time).observe(haveBall)

				val possessProb: Probability =
					VariableElimination.probability(possessionVar(currentTime), HAVE_BALL_AT_CURR_TIME)


				// Keeping all observations INDEPENDENT, while keeping the dependency of immediate past, when it
				// appears. Effect is just to "unobserve" the times that are not the immediate past.
				if(time != immediatePast){
					possessionVar(time).unobserve() // the separate / non-cumulative aspect
				}

				probs += possessProb

			}

			val timeProbPairs: List[(DiscreteTime, Probability)] = shuffledTimes.zip(probs).toList


			// TESTING

			// Testing 1: check the prior prob is not the same with EACH of the other possession probs
			val arePriorAndObservedProbsPairwiseDifferent: Boolean =
				probs.forall(obsProb => notAllSame(obsProb, priorProb) || notAllSameWithTolerance(obsProb, priorProb))


			// Testing 2: assert that all the observations made before currentime will be different (not all same)
			val nonMarkovTimeProbs: List[(DiscreteTime, Probability)] = timeProbPairs
				.takeWhile{case (time, prob) => time != immediatePast}


			val nonMarkovProbs: List[Probability] = nonMarkovTimeProbs.map(_._2)

			val markovTimeProbs: List[(DiscreteTime, Probability)] = timeProbPairs
				.dropWhile { case (time, prob) => time != immediatePast } // next after dropped was observed at
			// immediate past

			val markovProbs: List[Probability] = markovTimeProbs.map(_._2)


			val areNonMarkovsDifferent: Boolean =
				notAllSame(nonMarkovProbs:_*) || notAllSameWithTolerance(nonMarkovProbs:_*)


			// Testing 3: assert that all observations made after the immediate time after current will have same
			val areMarkovsEqual: Boolean = equalWithTolerance(markovProbs:_*)


			// LOGGING

			// Logging where the non-markov probabilities are (before observing at immediate past)
			nonMarkovTimeProbs.foreach{
				case (time, prob) =>
					Logger.log()(false)((s"Probability of possession at t = $currentTime " +
						s"| observe possession at t = $time", prob) )
			}

			// Logging where the markov probabilities are (after an on observing at immediate past)
			markovTimeProbs.foreach{
				case (time, prob) =>
					Logger.log()(false)((s"Probability of possession at t = $currentTime " +
						s"| observe possession at t = $time, ($immediatePast)", prob) )
			}

			// Logging the results of the tests
			Logger.log()()(
				("timeProbPairs", timeProbPairs),
				("areNonMarkovsDifferent", areNonMarkovsDifferent),
				("areMarkovsEqual", areMarkovsEqual)
			)

			counter += 1


			arePriorAndObservedProbsPairwiseDifferent &&
				areNonMarkovsDifferent &&
				areMarkovsEqual
	}




	// Test: inclusive / immediate time past (<= currentTime - 1)

	val testINClusiveDEPendentObservationsObeyMarkovAssumption = forAll(genCurrentTime, genHaveBall) {

		(currentTime: Int, haveBall: Boolean) =>


			// Declaring some variables as state for below tests:

			//val CURRENT_TIME: Int = 5
			val immediatePast: Int = currentTime - 1
			val earlierPasts: List[Int] = (0 to (immediatePast - 1)).toList



			Logger.log()()(("\n---------------- ITERATION", counter))


			// Create the markov chain
			// length CHAIN_LENGTH, from 0 ... CHAIN_LENGTh-1
			val possessionVar: Array[Element[Boolean]] = createMarkovChain(length = CHAIN_LENGTH)

			// Create list to store the observed probabilities of soccer ball possession at current time, after
			// observations
			// Contains list of list, where inner list is specific to how the observations happened in time.
			//val probs: List[ListBuffer[Probability]] = List.fill[ListBuffer[Probability]](NUM_WAYS_TO_OBSERVE)(ListBuffer())
			val probs: ListBuffer[Probability] = ListBuffer()


			// Create list of points in time such that they are less than current time (so in the past) but also
			// strictly less than the current time (so 0,1,2,3 for currtime = 5)
			val inclusivePastTimes: Seq[DiscreteTime] = earlierPasts :+ immediatePast


			// Get the prior probability of possession (must do this BEFORE doing observations in for loop below)
			val priorProb: Probability =
				VariableElimination.probability(possessionVar(currentTime), HAVE_BALL_AT_CURR_TIME)


			// Shuffle the times so we can observe the ball posession at random order of times.
			val shuffledTimes = Random.shuffle(inclusivePastTimes)

			for { time <- shuffledTimes } {

				// All observations are dependent, so don't say unobserve after any of these observations.
				possessionVar(time).observe(haveBall)

				val possessProb: Probability =
					VariableElimination.probability(possessionVar(currentTime), HAVE_BALL_AT_CURR_TIME)


				probs += possessProb

			}

			//val timeProbPairs: Seq[(DiscreteTime, Probability)] = shuffledTimes.zip(probs)

			// NOTE: dependency aspect of observations
			// First, creating a way to display the accumulated times, based on dependencies from the for loop:
			val accumulatedTimes: Seq[Seq[DiscreteTime]] =
			shuffledTimes
				.scanLeft(Seq[DiscreteTime]())((accList, newTime) => accList :+ newTime)
				.tail

			val accTimeProbPairs: Seq[ (Seq[DiscreteTime], Probability) ] = accumulatedTimes.zip(probs)


			// TESTING

			// Testing 1: check the prior prob is not the same with EACH of the other possession probs
			val arePriorAndObservedProbsPairwiseDifferent: Boolean =
				probs.forall(obsProb => notAllSame(obsProb, priorProb) || notAllSameWithTolerance(obsProb, priorProb))


			// Testing 2: assert that all the observations made before currentime will be different (not all same)

			// Comparing the last time in the list of the accumulated times, in each times-prob-pair, since the
			// last of the accumulated times is the current one represented in the for loop.
			val nonMarkovTimeProbs: Seq[(Seq[DiscreteTime], Probability)] =
			accTimeProbPairs
				.takeWhile{case (accumTimes, prob) => accumTimes.last != immediatePast}

			val markovTimeProbs: Seq[(Seq[DiscreteTime], Probability)] =
				accTimeProbPairs
					.dropWhile { case (accumTimes, prob) => accumTimes.last != immediatePast }

			val nonMarkovProbs: Seq[Probability] = nonMarkovTimeProbs.map(_._2)
			val markovProbs: Seq[Probability] = markovTimeProbs.map(_._2)




			val areNonMarkovsDifferent: Boolean =
				notAllSame(nonMarkovProbs:_*) || notAllSameWithTolerance(nonMarkovProbs:_*)


			// Testing 3: assert that all observations made after the immediate time after current will have same
			val areMarkovsEqual: Boolean = equalWithTolerance(markovProbs:_*)


			// LOGGING

			// Logging where the non-markov probabilities are (before observing at immediate past)
			nonMarkovTimeProbs.foreach{
				case (accumTimes, prob) =>
					Logger.log()(false)((s"Probability of possession at t = $currentTime " +
						s"| observe possession at t = ${accumTimes.mkString(",")}", prob) )
			}

			// Logging where the markov probabilities are (after an on observing at immediate past)
			// Writing a () around the time if it is the immediate-past time (immediately before currentTime) else
			// not.
			markovTimeProbs.foreach{
				case (accumTimes, prob) =>
					Logger.log()(false)((s"Probability of possession at t = $currentTime " +
						s"| observe possession at t = ${accumTimes
							.map( t => if (t == immediatePast) s"($t)" else s"$t" )
							.mkString(",")}", prob) )
			}


			// Logging the results of the tests
			Logger.log()()(
				("timeProbPairs", accTimeProbPairs.map { case(accTimes, prob) => (accTimes.last, prob)}),
				("areNonMarkovsDifferent", areNonMarkovsDifferent),
				("areMarkovsEqual", areMarkovsEqual)
			)


			counter += 1


			arePriorAndObservedProbsPairwiseDifferent &&
				areNonMarkovsDifferent &&
				areMarkovsEqual
	}

}


object MarkovSoccerChainPropertyChecker extends Properties("MarkovAssumption") {

	import MarkovSoccerChainProps._




	/*Console.println("\n(1) exclusive + dependent")
	testEXClusiveDEPendentObservationsDoNotObeyMarkovAssumption.check //(50)
	resetCounter()

	Console.println("\n(2) exclusive + independent")
	testEXClusiveINDependentObservationsDoNotObeyMarkovAssumption.check //(50)
	resetCounter()

	Console.println("\n(3) inclusive + dependent")
	testINClusiveDEPendentObservationsObeyMarkovAssumption.check()
	resetCounter()*/

	Console.println("\n(4) inclusive + independent")
	testINClusiveINDependentObservationsObeyMarkovAssumption.check()
	resetCounter()

	// TODO better way to weave in this state? Try passing it to property methods directly?



	//NOTES:
	// 1) markov chain (curr time) needs to be longer for us to see BIG ENOUGH differences in the numbers when not
	// observing first the next immediate state (the larger the chain, the more similar the numbers are even before
	// the immediate state is observed, so harder to see if the test of immediacy is achieved). Test smaller and
	// larger markov chains (5 vs length 90)
	// ----> if length <= 5 ASSERT that  smallest difference between numbers is LARGER than largest difference
	//  between numbers when length >= 90

	// 2) need to test observing the immediate state, then see how similar the numbers are. (use approx equal for
	// this test)

	// 3) can use incr + order and random order and decr + order only after first conditioning on the immediate next
	// state.

	// 4) can then test any order but check specifically: AFTER the next immediate state is observed, ASSERT that
	// all the consequent probabilities are APPROX EQUAL.
	// and that in total everything is NOT ALL SAME.
	// and that BEFORE everything is NOT ALL SAME.

	// 5) for non-immediate tests, assert the NOT ALL SAME numbers test, all the way.


	// Choose the number above (future) the current time in a dynamic way ...
	// ... for non-immediate future (> currentTime + 1)
	// ... for immediate future  (>= currentTime + 1)
}


