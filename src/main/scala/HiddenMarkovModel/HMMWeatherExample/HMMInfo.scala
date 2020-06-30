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
	 * Given the rows of the transition matrix, this produces a function of type Element[H] => Element[H]
	 */
	def transition(rainyTransition: Array[Double], sunnyTransition: Array[Double])(e: Element[H]): Element[H] = {


		/**
		 * Gives the distribution over hidden states at time t given the state at time (t-1)
		 * https://hyp.is/NweblroCEeq29IOl4FccPA/mioalter.wordpress.com/2016/02/13/hmm-hidden-markov-models-with-figaro/
		 * @param h
		 * @return
		 */
		def transitionGivenState(h: H) : Element[H] = h match {
			case Rainy =>
				Select(
					rainyTransition(0) -> Rainy,
					rainyTransition(1) -> Sunny
				)
			case Sunny =>
				Select(
					sunnyTransition(0) -> Rainy,
					sunnyTransition(1) -> Sunny
				)
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
				Select(
					rainyEmission(0) -> Shop,
					rainyEmission(1) -> Clean,
					rainyEmission(2) -> Walk
				)

			case Sunny =>
				Select(
					sunnyEmission(0) -> Shop,
					sunnyEmission(1) -> Clean,
					sunnyEmission(2) -> Walk
				)
		}

		e.flatMap(h => emissionGivenState(h))

	}


	/**
	 *
	 * Makes a sequence of hidden states in which each hidden state is obtained from
	 * the previous one by applying the transition matrix.
	 *
	 * @param length
	 * @param transitionMatrix
	 * @param aprioriArray
	 * @return
	 */
	def makeHiddenSequence(length: Int, transitionMatrix: Element[H] => Element[H],
					   aprioriArray: Array[Double]) : List[Element[H]] = {


		//Creating the apriori distribution over hidden states.
		val aprioriDist = Select(
			aprioriArray(0) -> Rainy,
			aprioriArray(1) -> Sunny
		)


		def buildSequence(n: Int, elems: List[Element[H]]): List[Element[H]] = {
			(n, elems) match {
				// Step 3: when we get down to n = 0, we are done
				case (0, elems) => elems
				// Step 1: start with the apriori distribution
				case (n, Nil) => buildSequence(n - 1, List(aprioriDist))
				// Step 2: apply the transitionMatrix argument to the head and attach it onto the list
				case (n, x :: xs) => buildSequence(n - 1, transitionMatrix(x) :: x :: xs)
			}
		}

		// reverse the result of hidden states so time increases from left to right
		buildSequence(length, Nil).reverse 
	}

}
