package utils

/**
 *
 */
object Logger {

	def log(tag: String = "")(skipLineAfterTag: Boolean = true)(messageItems: (String, Any)*): Unit = {

		val strMsg: String = messageItems.map {case (str, any) => s"> $str: \t $any"}.mkString("\n\t")

		if(! tag.isEmpty){ // then need to always skip line after non-empty tag, so skip line is essentially true here
			Console.println(s"$tag \n\t$strMsg")

		} else { // else if the tag IS empty, then no need to skip the line
			Console.println(s"$tag \t$strMsg")
		}
	}
}
