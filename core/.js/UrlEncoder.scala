package java.net

import com.github.ghik.silencer.silent

object URLEncoder {

  @silent("implicit numeric widening")
  def encode(s: String, @silent("never used") encoding: String = "UTF-8"): String = {
    s flatMap { char => char match {
      case ' ' => "+"
      case '-' | '_' | '.' | '!' | '~' | '*' | '\'' | '(' | ')' => s"$char"
      case alpha if alpha.isLetterOrDigit => s"$alpha"
      case reserved => "%" ++ reserved.toHexString.toUpperCase
    }}
  }
}
