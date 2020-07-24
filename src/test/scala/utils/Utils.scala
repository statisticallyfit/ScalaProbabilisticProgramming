package utils

/**
 *
 */
object Utils {


	final val TOLERANCE: Double = 0.00000001


	/**
	 * Simple check to see if the given elements are all distinct or not
	 * @param elements
	 * @return
	 */
	def allDifferent(elements: Double*): Boolean = {
		List(elements).distinct.length == List(elements).length
	}

	def approximatelyEqual(num1: Double, num2: Double, precision: Double): Boolean = if ( (num1 - num2).abs < precision)
		true else false


	def approxEqual(xs: Double*)(implicit precision: Double = TOLERANCE): Boolean = {
		xs.combinations(n = 2).forall{ case Seq(e1, e2) => approximatelyEqual(e1, e2, precision = precision)}
	}

	/**
	 * Checks that all elements are either never equal to one another, or there are some equal ones among them but
	 * there is at least one pair that are not equal.
	 *
	 * Never returns TRUE when all the elements are equal
	 *
	 * @param xs
	 * @param precision
	 * @return
	 */
	def notAllSame(xs: Double*)(implicit precision: Double = TOLERANCE): Boolean = {

		// First do error checking: if empty passed list, then say TRUE anyway:
		if(xs.toList.isEmpty) return true

		// Else continue to evaluate

		// Contains elements true or false indicating if the pairs in that location were equal (with tolerance) or not.
		val pairsEqual: List[Boolean] = xs.combinations(n = 2).map{
			case Seq(e1, e2) => approximatelyEqual(e1, e2, precision = precision )
		}.toList

		//val wasNonEqualPair: Boolean = pairsEqual.exists(isPairEqual => !isPairEqual) // does there exist at least one
		// pair that wasn't equal?

		val wereAllPairsEqual: Boolean = pairsEqual.foldLeft(true)(_ && _) // .reduceLeft(_ && _ )

		! wereAllPairsEqual
	}
}
