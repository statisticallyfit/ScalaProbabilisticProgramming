package utils

/**
 *
 */
object Logger {

	def log(tag: String = "")(skipLineAfterTag: Boolean = true)
		  /*(useIndent: Boolean = false)*/
		  (messageItems: (String, Any)*): Unit	= {

		// case 1: multiple items, then use the indent and arrows.
		// case 2: one item, then by default do not use indent and arrows

		val doUseIndent: Boolean = if(messageItems.length == 1) false else true //useIndent
		val doSkipLineAfterTag: Boolean = if(! tag.isEmpty) true else skipLineAfterTag

		//val strMsg: String = messageItems.map {case (str, any) => s"> $str: \t $any"}.mkString("\n\t")



		if(doSkipLineAfterTag){ // then need to always skip line after non-empty tag, so skip line is essentially true here

			if(doUseIndent){
				val strMsg: String = messageItems.map {case (str, any) => s"> $str: \t $any"}.mkString("\n\t")
				Console.println(s"$tag \n\t$strMsg")
			} else { // no indent, no arrow
				val strMsg: String = messageItems.map {case (str, any) => s"$str: \t $any"}.mkString("\n\t")
				Console.println(s"$tag \n$strMsg")
			}

		} else { // else if the tag IS empty, then no need to skip the line

			if(doUseIndent){
				val strMsg: String = messageItems.map {case (str, any) => s"> $str: \t $any"}.mkString("\n\t")
				Console.println(s"$tag \t$strMsg")
			} else {
				val strMsg: String = messageItems.map {case (str, any) => s"$str: \t $any"}.mkString("\n\t")
				Console.println(s"$tag $strMsg")
			}
		}
	}
}
