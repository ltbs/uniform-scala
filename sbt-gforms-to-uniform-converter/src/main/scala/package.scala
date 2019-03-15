package ltbs.uniform

package object gformsparser extends gformsparser.GformsInflector {

  type URL = String

  implicit class RichFieldList(val fl: List[Field]) extends AnyVal {
    def flattenGroups: List[Field] = fl.flatMap { f =>
      f match {
        case g: GroupField => g.fields.map{_.updateId(f.id ++ "-" ++ _)}.flattenGroups
        case o        => List(o)
      }
    }
  }

}

package gformsparser {

  trait GformsInflector extends StringInflector {
    val badWords: List[String] = List("details", "of", "your", "tell", "us", "about", "what", "this", "the", "who", "is", "are")
  }
}
