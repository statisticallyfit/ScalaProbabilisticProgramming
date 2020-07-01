package HiddenMarkovModel.HMMWeatherExample



import com.cra.figaro.language._


import HMMTypes._


/**
 * Tutorial source = https://github.com/mioalter/fp-scala/blob/master/hmm-example/code/src/main/scala/hmm/hmm.scala
 */
object HMMBuilder {


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
	def makeHiddenSequence(length: Int,
					   transitionMatrix: Element[H] => Element[H],
					   aprioriArray: Array[Double]) : List[Element[H]] = {


		//Creating the apriori distribution over hidden states.
		val aprioriDist: Element[H] = Select(
			aprioriArray(0) -> Rainy,
			aprioriArray(1) -> Sunny
		) // note Element[H] is some kind of supertype of /*AtomicSelect[HiddenState]*/


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


	/**
	 * Makes the sequence of observable states that are corresponding to the sequence of the hidden States.
	 * Created by just applying the emission matrix to each hidden state.
	 *
	 * @param hiddenSequence
	 * @param emissionMatrix
	 * @return
	 */
	def makeObservableSequence(hiddenSequence: List[Element[H]],
						  emissionMatrix: Element[H] => Element[O]): List[Element[O]] = {

		hiddenSequence.map(h => emissionMatrix(h))
	}


	/**
	 * Putting the above information into a class and companion object to make an HMM with given or
	 * default parameters
	 */
	class HiddenMarkovModel(val length: Int
					    , val aprioriDist: Array[Double]
					    , val rainyTransmission: Array[Double]
					    , val sunnyTransmission: Array[Double]
					    , val rainyEmission: Array[Double]
					    , val sunnyEmission: Array[Double]
					   ) {

		//TODO call this hiddenSeq or hiddenState? are these hidden STATES? original or after??

		val hidden: List[Element[H]] =

			makeHiddenSequence(
				length = length,
				transitionMatrix = transition(rainyTransmission, sunnyTransmission),
				aprioriArray = aprioriDist
			)


		val observable: List[Element[O]] =

			makeObservableSequence(
				hiddenSequence = hidden,
				emissionMatrix = emission(rainyEmission, sunnyEmission)
			)


	}


	object HiddenMarkovModel {

		val aprioriDist: Array[Double] = Array(0.4, 0.6)
		val rainyTransmission: Array[Double] = Array(0.6, 0.4)
		val sunnyTransmission: Array[Double] = Array(0.3, 0.7)
		val rainyEmission: Array[Double] = Array(0.2, 0.7, 0.1)
		val sunnyEmission: Array[Double] = Array(0.4, 0.1, 0.5)


		def apply(length: Int): HiddenMarkovModel =

			new HiddenMarkovModel(
				length,
				aprioriDist,
				rainyTransmission,
				sunnyTransmission,
				rainyEmission,
				sunnyEmission
			)

		def apply(length: Int,
				aprioriDist: Array[Double],
				rainyTransmission: Array[Double],
				sunnyTransmission: Array[Double],
				rainyEmission: Array[Double],
				sunnyEmission: Array[Double]): HiddenMarkovModel =

			new HiddenMarkovModel(
				length,
				aprioriDist,
				rainyTransmission,
				sunnyTransmission,
				rainyEmission,
				sunnyEmission
			)
	}
}
