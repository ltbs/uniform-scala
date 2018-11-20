package ltbs.uniform

package object gformsparser {
  implicit class RichFieldList(val fl: List[Field]) extends AnyVal {
    def flattenGroups: List[Field] = fl.flatMap { f =>
      f match {
        case g: GroupField => g.fields.map{_.updateId(f.id ++ "-" ++ _)}.flattenGroups
        case o        => List(o)
      }
    }
  }
}
