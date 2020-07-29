package utils

/**
 *
 */
object Utils {


	final val TOLERANCE: Double = 1E-15 // (fourteen zeros) //1.0E-6 // tested with getMaxTol function
	// NOTE: lower tolerance makes it easier for nums to be approx equal, imposes less strict restriction.





	def twoApproxEqual(num1: Double, num2: Double, precision: Double = TOLERANCE): Boolean = {
		//Console.println(s"approxEqual(): Comparing num1 = $num1 and num2 = $num2")

		if ( (num1 - num2).abs < precision)
			true else false
	}


	def equalWithTolerance(xs: Double*)(implicit precision: Double = TOLERANCE): Boolean = {

		// First error check
		if(xs.isEmpty || xs.length == 1) return true

		// Else continue checking
		xs.combinations(n = 2).forall{ case Seq(e1, e2) => twoApproxEqual(e1, e2, precision = precision)}
	}

	/**
	 * Gets the maximum / largest value of tolerance to use by comparing set of numbers that are assumed to be close
	 * enough and getting their maximum difference, and returning a number that has as many front zeroes as that of
	 * their max difference.
	 * @param xs
	 * @return
	 */
	def getMaxTol(xs: Double*): Double = {
		xs.combinations(n = 2).map { case Seq(num1, num2) => (num1 - num2).abs }.max
	}



	/**
	 * Checks: at least ONE pair of elements (from two-way combinations) are different from each other (strictly, no
	 * tolerance)
	 *
	 * Checks that not all the elements are the same (with no tolerance, so if find any strictly different then
	 * returns true else false.
	 *
	 * @param elements
	 * @return
	 */
	def notAllSame(elements: Double*): Boolean = {
		if(elements.toList.isEmpty || elements.length ==1) return true

		// Else do the main logic
		List(elements).distinct.length == List(elements).length
	}


	/**
	 * Checks that all elements are either never equal to one another, or there are some equal ones among them but
	 * there is at least one pair that are not equal.
	 *
	 * Never returns TRUE when all the elements are equal
	 *
	 * Checks there is at least ONE pair of elements (from two-way combinations) that are NOT equal to each other
	 * (with tolerance)
	 *
	 * @param xs
	 * @param precision
	 * @return
	 */
	def notAllSameWithTolerance(xs: Double*)(implicit precision: Double = TOLERANCE): Boolean = {

		// First do error checking:

		// If empty passed list or if just one element, then say TRUE anyway:
		if(xs.toList.isEmpty || xs.length == 1) return true

		// Else continue to evaluate

		// Contains elements true or false indicating if the pairs in that location were equal (with tolerance) or not.
		val pairsEqual: List[Boolean] = xs.combinations(n = 2).map{
			case Seq(firstNumber, secondNumber) => twoApproxEqual(firstNumber, secondNumber, precision = precision )
		}.toList

		//val wasNonEqualPair: Boolean = pairsEqual.exists(isPairEqual => !isPairEqual) // does there exist at least one
		// pair that wasn't equal?

		val wereAllPairsEqual: Boolean = pairsEqual.forall(pair => pair)

		! wereAllPairsEqual
	}
}
