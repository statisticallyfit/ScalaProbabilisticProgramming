package Blogs


import breeze.stats.{distributions => bdist}
import breeze.linalg.DenseVector

/**
 * Tutorial source from = https://darrenjw.wordpress.com/2019/08/07/write-your-own-general-purpose-monadic-probabilistic-programming-language-from-scratch-in-50-lines-of-scala-code/
 */

object ParticleInfo {
	/**
	 * There is a default number of particles for sampling and resampling.
	 */
	implicit val NUM_PARTICLES = 300
}
import ParticleInfo._



case class Particle[T](value: T, logWeight: Double) {

	/**
	 * The map method pushes a particle through a transformation.
	 * @param f
	 * @tparam S
	 * @return
	 */
	def map[S](f: T => S): Particle[S] = Particle(value = f(value), logWeight = logWeight)

}



/**
 * The main probability monad
 *
 * @tparam T
 */
trait Prob[T] {

	val particles: Vector[Particle[T]]

	/**
	 * Apply the given function f to each particle in the vector of particles.
	 * @param f
	 * @tparam S
	 * @return
	 */
	def map[S](f: T => S): Prob[S] = Empirical(particles.map(p => p.map(f)))

	/**
	 * The monad bind function.
	 * Forms the naive product of empirical measures and then resamples in order to stop an explosion in the number
	 * of particles.
	 * @param g
	 * @tparam S
	 */
	def flatMap[S](g: T => Prob[S]): Prob[S] = {
		Empirical(particles = particles.map( p => {
			g(p.value) //yields instance of Prob[S] monad
				.particles //get Prob[S]'s particles vector
				// for each particle,  create new particle as follows
				.map(q => Particle(value = q.value, logWeight = p.logWeight + q.logWeight))
		}).flatten) //flatten into on Prob[S] from multiple Prob[S]
			.resample  //resample on the resulting Prob[S]
	}


	/**
	 * Resample method to avoid explosion in the number of particles.
	 *
	 * 		1) Using a log-sum-exp trick to avoid overflow and underflow when the log weights are exponentiated.
	 *
	 * 		2) Although resample returns an equally weighted set of particles, the log weights are all set so that
	 * 		the average raw weight of the output set matches the average raw weight of the input set.  This is
	 * 		necessary to correctly propagate conditioning information back through multiple monadic binds
	 * 		(flatmaps). The cond method allows conditioning of a distribution
	 *
	 * @param N
	 * @return
	 */
	def resample(implicit N: Int): Prob[T] = {

		val logWeights = particles.map(p => p.logWeight)
		val maxLogWeight = logWeights.reduce(math.max(_, _))
		val rawWeights = logWeights.map(lw => math.exp(lw - maxLogWeight))
		val law = maxLogWeight + math.log(rawWeights.sum / rawWeights.length)
		val ind = bdist.Multinomial( DenseVector(rawWeights.toArray) ).sample(N)
		val newParticles = ind.map(i => particles(i))

		Empirical(particles = newParticles.toVector map(p => Particle(value = p.value, logWeight = law)))
	}

	/**
	 * Allows conditioning of a distribution using an arbitrary log-likelihood.
	 * Included for comparison with some other implementations (referred to later). Not using it here though.
	 * @param logLik
	 * @return
	 */
	def cond(logLik: T => Double): Prob[T] =
		Empirical(particles = particles.map(p =>
			Particle(value = p.value,
				logWeight = p.logWeight + logLik(p.value))))


	/**
	 * Extracts an unweighted set of values from a distribution for subsequent analysis.
	 * @return
	 */
	def empirical: Vector[T] = resample.particles.map(_.value)


	/**
	 * Turns some unweighted particles into a set of particles with equal weights (like a kind of inverse to the
	 * empirical method)
	 *
	 * @param ts
	 * @param logWeight
	 * @tparam T
	 * @return
	 */
	def unweighted[T](ts: Vector[T], logWeight: Double = 0.0): Prob[T] =
		Empirical(particles = ts.map(t => Particle(value = t, logWeight = logWeight)))

}




/**
 * Empirical is the simplest implementation of the base probability monad trait, Prob[T]
 * Empirical is a collection of weighted particles.
 * @param particles
 * @tparam T
 */
case class Empirical[T](particles: Vector[Particle[T]]) extends Prob[T]




trait Dist[T] extends Prob[T] {

	/**
	 *
	 * @param obs
	 * @return
	 */
	def logLikelihood(obs: T): Double

	/**
	 * Calculates log likelihood of each observation and then sums up the result
	 *
	 * @param observations
	 * @return
	 */
	def logLikelihood(observations: Seq[T]): Double = observations
		.map(obs => logLikelihood(obs))
		.reduce( _ + _ )

	/**
	 * Re-weights a particle set according to the observed log-likelihood. In other words, the inner particles'
	 * logWeight is calculated from log likelihood of passed observations.
	 *
	 * TODO (meaning?): it also returns a particle cloud representing the posterior-predictive distribution of and
	 * IID value from the same distribution. Handy, but expensive since introduces another particle cloud. So if you
	 * aren't interested in the posterior predictive, use fitQ (fit quick) which doesn't return anything useful.
	 *
	 * @param observations
	 * @return
	 */
	def fit(observations: Seq[T]): Prob[T] =
		Empirical(particles = particles.map(p =>
			Particle(value = p.value, logWeight = p.logWeight + logLikelihood(observations))))

	def fitQ(observations: Seq[T]): Prob[T] =
		Empirical(particles = Vector(
			Particle(value = observations.head, logWeight = logLikelihood(observations))
		))

	def fit(obs: T): Prob[T] = fit(List(obs))

	def fitQ(obs: T): Prob[T] = fitQ(List(obs))
}


//Adding  a few standard distributions
case class Normal(mu: Double, sigmaSq: Double)(implicit N: Int) extends Dist[Double]{

	//Implementing particles from Prob[T]
	lazy val particles: Vector[Particle[Double]] =
		unweighted(ts =
			bdist.Gaussian(mu = mu, sigma = math.sqrt(sigmaSq)).sample(N).toVector
		).particles


	// Implementing log lik from Dist[T]
	def logLikelihood(obs: Double): Double =
		bdist.Gaussian(mu = mu, sigma = math.sqrt(sigmaSq)).logPdf(obs)

}


case class Gamma(a: Double, b: Double)(implicit N: Int) extends Dist[Double] {

	// Implementing particles from Prob[T]
	lazy val particles: Vector[Particle[Double]] =
		unweighted(ts =
			bdist.Gamma(shape = a, scale = 1.0 / b).sample(N).toVector
		).particles

	// Implementing log lik from Dist[T]
	def logLikelihood(obs: Double): Double =
		bdist.Gamma(shape = a, scale = 1.0 / b).logPdf(obs)

}


case class Poisson(mu: Double)(implicit N: Int) extends Dist[Int] {

	// Implementing particles from Prob[T]
	lazy val particles: Vector[Particle[Int]] =
		unweighted(ts =
			bdist.Poisson(mean = mu).sample(N).toVector
		).particles

	// Implementing log lik from Dist[T]
	def logLikelihood(obs: Int): Double =
		bdist.Poisson(mean = mu).logProbabilityOf(obs)

}