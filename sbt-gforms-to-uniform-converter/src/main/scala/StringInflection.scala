package ltbs.uniform.gformsparser

trait StringInflector {

  val badWords: List[String]

  lazy val lowerBad = badWords.map(_.toLowerCase)

  implicit class ListInflector(in: List[String]) {

    private def sterilizeWord(in: String): String = in.filter{x => x.isLetter || x.isDigit}
    private def titleWord(word: String): String = sterilizeWord(word) match {
      case c if c.nonEmpty => c.head.toUpper.toString ++ c.drop(1)
      case _ => ""
    }

    private def lowerTitleWord(word: String): String = sterilizeWord(word) match {
      case c if c.nonEmpty => c.head.toLower.toString ++ c.drop(1)
      case _ => ""
    }

    private def debad(in: List[String]): List[String] =
      in.filterNot{x => lowerBad.contains(x.toLowerCase)}

    def camel: String = debad(in).map(titleWord).mkString

    def lowerCamel: String = debad(in) match {
      case (h::t) => {lowerTitleWord(h) :: t.map(titleWord)}.mkString      
      case _ => ""
    }

    def kebab: String = debad(in) match {
      case (h::t) => {lowerTitleWord(h) :: t.map(lowerTitleWord)}.mkString("-")
      case _ => ""
    }

    def underscore: String = debad(in) match {
      case (h::t) => {lowerTitleWord(h) :: t.map(lowerTitleWord)}.mkString("_")
      case _ => ""
    }
  }
}
