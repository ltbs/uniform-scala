package java.net

object URLEncoder {

  def encode(s: String, encoding: String = "UTF-8"): String = {
    s flatMap { char => char match {
      case ' ' => "+"
      case '-' | '_' | '.' | '!' | '~' | '*' | '\'' | '(' | ')' => s"$char"
      case alpha if alpha.isLetterOrDigit => s"$alpha"
      case reserved => "%" ++ reserved.toHexString.toUpperCase
    }}
  }
}
