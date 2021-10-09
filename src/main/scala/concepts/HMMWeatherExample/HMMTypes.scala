package concepts.HMMWeatherExample

/**
 * Tutorial source = https://mioalter.wordpress.com/2016/02/13/hmm-hidden-markov-models-with-figaro/
 */
object HMMTypes {

	sealed trait HiddenState
	case object Rainy extends HiddenState
	case object Sunny extends HiddenState

	sealed trait ObservableState
	case object Shop extends ObservableState
	case object Clean extends ObservableState
	case object Walk extends ObservableState
}
