package ProbabilityMonads.Particle

/**
 *
 */
import breeze.stats.{meanAndVariance => calcMeanVar}
import breeze.stats.meanAndVariance.MeanAndVariance



import ParticleInfo._ //getting the implicit parameter NUM_SAMPLES for N



/**
 * Example: bayesian inference for the mean and variance of a normal distribution from a random sample, using the
 * probability monad we declared.
 */

object NormalRandomSample extends App {

	val mod: Prob[(Double, Double)] = for {
		mu <- Normal(mu = 0, sigmaSq = 100)
		tau <- Gamma(a = 1, b = 0.1)
		_ <- Normal(mu = mu, sigmaSq = 1.0 / tau).fitQ(observations = List(8.0, 9, 7, 7, 8, 10))
	} yield (mu, tau)

	println(mod)

	val modEmpirical: Vector[(Double, Double)] = mod.empirical


	val sampledMean: MeanAndVariance = calcMeanVar(modEmpirical.map(_._1)) // mu
	println(s"sampledMean = $sampledMean")

	val sampledTau: MeanAndVariance = calcMeanVar(modEmpirical.map(_._2)) // tau
	println(s"sampledTau = $sampledTau")
}


/**
 * Inference for a discrete count given some noisy IID continuous measurements of it.
 */
object NoisyMeasurementsOfACount extends App {

	//TODO how does this structure relate to monad's flatMap? How are arguments passed in flatmap and what are the
	// arguments? What is the point of sending the last normal sampling to a wildcard?

	val mod: Prob[(Int, Double)] = for {
		count <- Poisson(mu = 10)
		tau <- Gamma(a = 1, b = 0.1)
		_ <- Normal(mu = count, sigmaSq = 1.0 / tau).fitQ(List(4.2, 5.1, 4.6, 3.3, 4.7, 5.3))
	} yield (count, tau)


	println(mod)

	val modEmpirical: Vector[(Int, Double)] = mod.empirical


	//TODO error "could not find implicit parameter for Impl in meanAndVariance.Impl --- but why does converting to
	// Double fix that?
	val sampledCount: MeanAndVariance = calcMeanVar(modEmpirical.map(_._1.toDouble)) // mu
	println(s"sampledCount = $sampledCount")

	val sampledTau: MeanAndVariance = calcMeanVar(modEmpirical.map(_._2.toDouble)) // tau
	println(s"sampledTau = $sampledTau")
}


/**
 * Bayesian estimation of a linear model
 */
object LinearModel extends App {

	// Create some data
	val x: List[Double] = List(1.0, 2, 3, 4, 5, 6)
	val y: List[Double] = List(3.0, 2, 4, 5, 5, 6)

	val xy = x zip y


	// Define a datatype representing a particular linear model
	case class Param(alpha: Double, beta: Double, residualVariance: Double)

	// Define prior distribution over models as follows
	val prior: Prob[Param] = for {
		alpha <- Normal(mu = 0, sigmaSq = 10)
		beta <- Normal(mu = 0, sigmaSq = 4)
		residualVariance <- Gamma(a = 1, b = 0.1)
	} yield Param(alpha, beta, residualVariance)

	println(s"prior = $prior")


	/**
	 * Our language doesn't include any syntactic support for fitting regression models so we define our own
	 * function for conditioning a distribution over models on a data point. (y is a single obs)
	 *
	 */
	def addPoint(current: Prob[Param], obs: (Double, Double)): Prob[Param] = for {
		p <- current // getting the inner param
		(x, y) = obs //todo meaning of saying EQUALS instead of ARROW symbol in a monad?
		_ <- Normal(mu = p.alpha + p.beta * x, sigmaSq = p.residualVariance).fitQ(obs = y)
	} yield p

	// Then apply the regression fitter to our prior as a fold over available data

	// The seed of foldleft is the prior
	// note: TYPE SIGNATURE: foldLeft(z: B)(f: (B, A) => B)
	val mod: Prob[Param] = xy.foldLeft(prior)((accParam, newObs) => addPoint(accParam, newObs))
	val modEmpirical: Vector[Param] = mod.empirical

	println(s"probParam after fold = $mod")
	println(s"probParam empirical = $modEmpirical")


	val sampledAlpha: MeanAndVariance = calcMeanVar(modEmpirical.map(_.alpha)) // alpha
	println(s"sampledAlpha = $sampledAlpha")

	val sampledBeta: MeanAndVariance = calcMeanVar(modEmpirical.map(_.beta)) // beta
	println(s"sampledBeta = $sampledBeta")

	val sampledResidualVariance: MeanAndVariance = calcMeanVar(modEmpirical.map(_.residualVariance)) // residual
	// variance
	println(s"sampledResidualVariance = $sampledResidualVariance")

}


/**
 * Dynamic generalized linear model.
 * Fitting a Poisson DGLM with log-link and simple brownian state evolution. The model is parameterized by an initial
 * state and evolution variance.
 */
object DynamicGLM extends App {


	val data = List(2, 1, 0, 2, 3, 4, 5, 4, 3, 2, 1)

	val prior: Prob[(Double, List[Double])] = for {

		evolutionVariance <- Gamma(a = 1, b = 1)
		initialState <- Normal(mu = 0.0, sigmaSq = 2.0)

	} yield (evolutionVariance, List(initialState))

	println(s"prior = $prior")


	/**
	 * Creates a new hidden state, prepends it to the list of hidden states, and conditions on the observed value at
	 * that time point, as follows:
	 */
	def addTimePoint(currentPrior: Prob[(Double, List[Double])],
				  obs: Int): Prob[(Double, List[Double])] = for {

		varAndStatesPair <- currentPrior
		(evolutionVar, states) = varAndStatesPair
		firstState = states.head
		normalState <- Normal(mu = firstState, sigmaSq = evolutionVar)
		_ <- Poisson(mu = math.exp(normalState)).fitQ(obs)

	} yield (evolutionVar, normalState :: states)


	// The seed of foldleft is the prior
	// note: TYPE SIGNATURE: foldLeft(z: B)(f: (B, A) => B)
	val mod: Prob[(Double, List[Double])] =
	data.foldLeft(prior)((accTuple, newObs) => addTimePoint(accTuple, newObs))

	val modEmpirical: Vector[(Double, List[Double])] = mod.empirical

	println(s"probTuple after fold = $mod")
	println(s"probTuple empirical = $modEmpirical")


	val sampledEvolutionVariance: MeanAndVariance = calcMeanVar(modEmpirical.map(_._1))
	println(s"sampledEvolutionVariance = $sampledEvolutionVariance")

	//TODO why reverse?
	val sampledInitialState: MeanAndVariance = calcMeanVar(modEmpirical.map(_._2.reverse.head))
	println(s"sampledInitialState = $sampledInitialState")

	val sampledFinalState: MeanAndVariance = calcMeanVar(modEmpirical.map(_._2.head))
	println(s"sampledFinalState = $sampledFinalState")
}