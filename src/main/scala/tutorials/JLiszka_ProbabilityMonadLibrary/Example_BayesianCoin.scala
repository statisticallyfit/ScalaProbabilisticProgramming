package tutorials.JLiszka_ProbabilityMonadLibrary

/**
 *
 */

import probability_monad.Distribution
import probability_monad.Distribution._

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
