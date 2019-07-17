package ltbs.uniform

object NonEmptyString {
  def fromString(in: String): Option[NonEmptyString] = in.trim match {
    case "" ⇒ None
    case x  ⇒ Some(shapeless.tag[NonEmptyStringTag][String](x))
  }

  def apply(in: String): NonEmptyString = fromString(in).getOrElse(
    throw new IllegalStateException("Empty string supplied")
  )
}
