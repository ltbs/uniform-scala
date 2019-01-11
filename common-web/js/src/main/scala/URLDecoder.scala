package java.net

object URLEncoder {

  def encodeChar(encoding: String)(c: Char): String = c match {
    case c if c >= 'a' && c <= 'z' => c.toString
    case c if c >= 'A' && c <= 'Z' => c.toString
    case c if c >= '0' && c <= '9' => c.toString
    case c if "-_.*".contains(c) => c.toString
    case ' ' => "+"
    case c => s"$c".getBytes(encoding).toList.map{
      i => "%" + i.toInt.toHexString.takeRight(2).toUpperCase
    }.mkString
  }

  @Deprecated
  def encode(input: String): String = encode(input, "UTF-8")

  def encode(input: String, encoding: String): String = input.flatMap(encodeChar(encoding))

}
