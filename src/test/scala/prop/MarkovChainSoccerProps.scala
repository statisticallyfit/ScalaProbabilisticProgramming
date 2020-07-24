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



object MarkovChainSoccerProps extends Properties("MarkovAssumption") {


	type DiscreteTime = Int
	type Observation = Boolean
	type Probability = Double


	CHAIN_LENGTH = 20

	val possessionVar: Array[Element[Boolean]] = createMarkovSoccerChain(length = CHAIN_LENGTH)


	// Choose the current time step dynamically (in the specs this was t = 5)
	val genCurrentTime: Gen[DiscreteTime] = Gen.choose(0, CHAIN_LENGTH)

	// Decide if we observe true or false (so observing happens, but need to know if possession of ball is true
	// or false)
	val genObserveYesOrNo: Gen[Observation] = Gen.oneOf(true, false)


	// Create list to store the observed probabilities of soccer ball possession at current time, after observations
	val listOfPossessProbs: ListBuffer[Probability] = ListBuffer()

	// Choose the number below (past) the current time in a dynamic way ...
	// ... for non-immediate past (< currentTime - 1)
	//val exclusivePastTime: Gen[DiscreteTime] = Gen.choose(0, genCurrentTime - 1)
	val propExclusiveSeparatePossessions = forAll(genCurrentTime, genObserveYesOrNo) {

		(currentTime: Int, yesOrNo: Boolean) =>


		// Create list of points in time such that they are less than current time (so in the past) but also
		// strictly less than the current time (so 0,1,2,3 for currtime = 5)
		val exclusivePastTimePoints: Seq[DiscreteTime] = (0 until (currentTime - 1))


		//exclusivePastTimePoints.foreach{ time =>
		for (time <- exclusivePastTimePoints) {

			possessionVar(time).observe(observation = yesOrNo)

			val possessProb: Probability = VariableElimination.probability(possessionVar(currentTime), true)

			listOfPossessProbs += possessProb // adding this to list

			possessionVar(time).unobserve() // doing the "separate" observation tactic
		}

		// The test: asserting that not all of these probabilities in the list should be the same:
		notAllSame(listOfPossessProbs:_*)

	}


	propExclusiveSeparatePossessions.check()

	// ... for immediate past (<= currentTime - 1)



	// Choose the number above (future) the current time in a dynamic way ...
	// ... for non-immediate future (> currentTime + 1)
	// ... for immediate future  (>= currentTime + 1)
}
