package ltbs.uniform
package examples.dst

import shapeless._, tag._
import cats.implicits._

trait ValidatedStringMixin {

  trait ValidatedString[A] {

    lazy val clazz = this.getClass.getName

    def validateAndTransform(in: String): Option[String]

    def apply(in: String) = 
      of(in).getOrElse{
        throw new IllegalArgumentException(
          s""""$in" is not a valid ${clazz.init}"""
        )
      }

    def of(in: String) =
      validateAndTransform(in) map {
        x => tag[A][String](x)
      }
  }

}
