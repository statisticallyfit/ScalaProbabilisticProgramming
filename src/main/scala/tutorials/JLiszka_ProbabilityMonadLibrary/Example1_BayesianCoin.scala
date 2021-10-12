package tutorials.JLiszka_ProbabilityMonadLibrary


import probability_monad.Distribution
import probability_monad.Distribution._




/**
 * PROBLEM TO SOLVE:
 *
 * You are given either a fair coin or a biased coin with equal probability.
 * If you flip it 5 times and it comes up heads each time, what is the probability you have
 * the fair coin?
 */


object Example1_BayesianCoin {
     
     case class Trial(haveFairCoin: Boolean, listOfCoinFlips: List[Coin])
     
     def bayesianCoin(numFlips: Int): Distribution[Trial] = {
          
          for {
               haveFairCoin <- tf() // monad op
               c = if (haveFairCoin) coin else biasedCoin(0.9) // assigning to value C (not monad op)
               flips <- c.repeat(numFlips) // monad op
          } yield Trial(haveFairCoin, flips)
     }
     
     final val NUM_FLIPS: Int = 5
}
import Example1_BayesianCoin._



object Runner_BayesianCoin extends App {
     
     val MY_NUM_REPEATS: Int = 10
     
     
     val runBayesianCoin =
          bayesianCoin(NUM_FLIPS)
            .given( (trial: Trial) => // filtering
                         trial.listOfCoinFlips.forall((coin: Coin) => coin == H))
            .pr(_.haveFairCoin) // probability
     
     Console.println(List.fill[Double](MY_NUM_REPEATS)(runBayesianCoin))
}
