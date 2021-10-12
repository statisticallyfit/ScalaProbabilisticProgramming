package tutorials.JLiszka_ProbabilityMonadLibrary


import probability_monad.Distribution
import probability_monad.Distribution._

/**
 * PROBLEM:
 *
 * You repeatedly roll a 6-sided die and keep a running sum.
 * What is the probability that the sum reaches exactly 30?
 */


object Example2_DieSum {

     def dieSum(numRolls: Int): Distribution[List[Int]] = {
          
          def markovFunction(runningSum: List[Int]): Distribution[List[Int]] = for {
               dieFace <- die
          } yield (dieFace + runningSum.head) :: runningSum
          
          
          always(value = List(0)).markov(numRolls)(markovFunction)
     }
     
     var NUM_ROLLS: Int = 30
     var TARGET_SUM: Int = 30
}
import Example2_DieSum._



object Runner_DieSum extends App {
     
     val MY_NUM_REPEATS: Int = 10
     
     val runDieSum: Int => Double = (numRolls: Int) =>
          dieSum(numRolls).pr((sums: List[Int]) => sums.contains(TARGET_SUM))
     
     Console.println(s"Probability that sums of rolls list contains $TARGET_SUM for $MY_NUM_REPEATS trials, for " +
                       s"$NUM_ROLLS rolls: " +
                       List.fill[Double](MY_NUM_REPEATS)(runDieSum(NUM_ROLLS)))
     
     NUM_ROLLS = 50
     Console.println(s"Probability that sums of rolls list contains $TARGET_SUM for $MY_NUM_REPEATS trials, for " +
                       s"$NUM_ROLLS rolls: " +
                       List.fill[Double](MY_NUM_REPEATS)(runDieSum(NUM_ROLLS)))
     
     NUM_ROLLS = 100
     TARGET_SUM = 60
     Console.println(s"Probability that sums of rolls list contains $TARGET_SUM for $MY_NUM_REPEATS trials, for " +
                       s"$NUM_ROLLS rolls: " +
                       List.fill[Double](MY_NUM_REPEATS)(runDieSum(NUM_ROLLS)))
     
}
