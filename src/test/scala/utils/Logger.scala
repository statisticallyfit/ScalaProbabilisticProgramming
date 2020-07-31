package utils

/**
 *
 */
object Logger {

	def log(tag: String,
		   skipLineAfterTag: Boolean = true,
		   generalMessage: String = "")(messageItems: (String, Any)*): Unit = {

		val strMsg: String = messageItems.map {case (str, any) => s"\t> $str: \t $any"}.mkString("\n")

		if(generalMessage.isEmpty){

			if(skipLineAfterTag){
				Console.println(s"$tag \n$strMsg")
			} else {
				Console.println(s"$tag $strMsg")
			}

		} else {
			Console.println(s"$tag \n$generalMessage \n$strMsg")
		}
	}
}
