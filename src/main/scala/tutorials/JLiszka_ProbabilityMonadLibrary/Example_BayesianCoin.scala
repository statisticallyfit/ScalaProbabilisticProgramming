package tutorials.JLiszka_ProbabilityMonadLibrary

/**
 *
 */

import probability_monad.Distribution
import probability_monad.Distribution._




/**
 * PROBLEM TO SOLVE:
 *
 * You are given either a fair coin or a biased coin with equal probability.
 * If you flip it 5 times and it comes up heads each time, what is the probability you have
 * the fair coin?
 */


object Example_BayesianCoin extends App {


  case class Trial(haveFairCoin: Boolean, flips: List[Coin])

  def bayesianCoin(nflips: Int): Distribution[Trial] = {


    for {
      haveFairCoin <- tf() // monad op
      c = if (haveFairCoin) coin else biasedCoin(0.9) // assigning to value C (not monad op)
      flips <- c.repeat(nflips) // monad op
    } yield Trial(haveFairCoin, flips)
  }

  val result = bayesianCoin(5).given(_.flips.forall(_ == H)).pr(_.haveFairCoin)

  println(result)
}
