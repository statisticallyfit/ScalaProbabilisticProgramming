package HiddenMarkovModel.HMMWeatherExample



import com.cra.figaro.language._


import Types._


/**
 * Tutorial source = https://github.com/mioalter/fp-scala/blob/master/hmm-example/code/src/main/scala/hmm/hmm.scala
 */
object HMMInfo {


	//The Figaro sampling needs this information
	type H = Product with Serializable with HiddenState
	type O = Product with Serializable with ObservableState


	/**
	 * TODO Takes conditional probabilities which define the transition matrix as input
	 */
	def transition(rainyTransition: Array[Double], sunnyTransition: Array[Double])(e: Element[H]): Element[H] = {

		def transitionGivenState(h: H) : Element[H] = h match {
			case Rainy => Select(rainyTransition(0) -> Rainy, rainyTransition(1) -> Sunny)
			case Sunny => Select(sunnyTransition(0) -> Rainy, sunnyTransition(1) -> Sunny)
		}

		e.flatMap(h => transitionGivenState(h))

	}


	/**
	 * TODO makes the emission matrix.
	 * Given the rows of the emission matrix, this produces a function (matrix) of type Element[H] => Element[O]
	 */
	def emission(rainyEmission: Array[Double], sunnyEmission: Array[Double])(e: Element[H]): Element[O] = {

		def emissionGivenState(h: H) : Element[O] = h match {
			case Rainy =>
				Select(rainyEmission(0) -> Shop,
					rainyEmission(1) -> Clean,
					rainyEmission(2) -> Walk)

			case Sunny =>
				Select(sunnyEmission(0) -> Shop,
					sunnyEmission(1) -> Clean,
					sunnyEmission(2) -> Walk)
		}

		e.flatMap(h => emissionGivenState(h))

	}
}
