package utils

/**
 *
 */
object Logger {

	def log(tag: String, generalMessage: String = "")(messageItems: (String, Any)*): Unit = {
		val strMsg: String = messageItems.map {case (str, int) => s"\t| $str: $int"}.mkString("\n")

		if(generalMessage.isEmpty){
			Console.println(s"\n$tag \n$strMsg")
		} else {
			Console.println(s"\n$tag \n$generalMessage \n$strMsg")
		}
	}
}
