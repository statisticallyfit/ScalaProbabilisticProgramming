package HiddenMarkovModel.HMMWeatherExample


import HMMTypes._
import HMMBuilder._
import HMMInference._
import com.cra.figaro.language._
import com.cra.figaro.algorithm.factored.VariableElimination
import com.cra.figaro.algorithm.OneTimeMPE
import com.cra.figaro.algorithm.factored.MPEVariableElimination


/**
  * Tutorial source = https://github.com/mioalter/fp-scala/blob/master/hmm-example/code/src/main/scala/hmm/examples.scala
  */


object VariableElimExample extends App {

	val hmm: HiddenMarkovModel = HiddenMarkovModel(length = 6)

	println("Probability it is Sunny on day = 2")
	println("Prior probability = " + VariableElimination.probability(target = hmm.hidden(2), value = Sunny))



	hmm.observable(2).observe(observation = Walk)

	println("Probability is is Sunny on day = 2 | After observing Walk on day 2: " +
		VariableElimination.probability(target = hmm.hidden(2), value = Sunny))



	hmm.observable(1).observe(observation = Walk)

	println("Probability of Sunny on day 1 | After observing Walk on day 1: " +
		VariableElimination.probability(target = hmm.hidden(2), value = Sunny))



	hmm.observable(0).observe(observation = Walk)

	println("Probability of Sunny on day 0 | After observing Walk on day 0: " +
		VariableElimination.probability(target = hmm.hidden(2), value = Sunny))




	hmm.observable(3).observe(observation = Walk)

	println("Probability of Sunny on day 3 | After observing Walk on day 3: " +
		VariableElimination.probability(target = hmm.hidden(2), value = Sunny))




	hmm.observable(4).observe(observation = Walk)

	println("Probability of Sunny on day 4 | After observing Walk on day 4: " +
		VariableElimination.probability(target = hmm.hidden(2), value = Sunny))




	hmm.observable(5).observe(observation = Walk)

	println("Probability of Sunny on day 5 | After observing Walk on day 5: " +
		VariableElimination.probability(target = hmm.hidden(2), value = Sunny))
}






object MostLikelyExample extends App {

	// Make an HMM of length 4 with the parameters from Stamp's Tutorial
	// Conversion from Stamp to this tutorial:
	// H = Sunny, C = Rainy
	// 0 = Clean, 1 = Shop, 2 = Walk

	val hmm: HiddenMarkovModel = HiddenMarkovModel(length = 4)

	//Make a sequence of observations
	val observationSeq: List[O] = List(Clean, Shop, Clean, Walk)

	// Register these observations in our HMM
	hmm.observable.zip(observationSeq).map{
		case (elemObs, obs) => elemObs.observe(observation = obs)
	}

	// Compute mostLikelys
	//val mostLikelyStates = mostlikelyHidd
}

