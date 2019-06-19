package ltbs.uniform
package common

import cats.syntax.eq._

package web {
  trait webcommon {

    type Path = List[List[String]]
    type JourneyConfig = String

    type DB = Map[List[String],String]

    def relativePath(from: List[String], to: List[String]): String = {
      import cats.instances.string._
      val (rem, add) = removeCommon(from, to)
        (rem.drop(1).map{_ => ".."} ++ add).mkString("/")
    }

    @annotation.tailrec
    final def removeCommon[B: cats.Eq](
      x: List[B],
      y: List[B]
    ): (List[B], List[B]) = (x,y) match {
      case (x::xs, y::ys) if x === y => removeCommon(xs, ys)
      case a => a
    }

    implicit def formToWebAsk[A, Html](
      implicit codec: FormFieldEncoding[A],
       renderer: FormFieldPresentation[A,Html]
    ): GenericWebAsk[A, Html] = new SimpleForm(InferFormField.combine(codec,renderer))

  }
}


package object web extends webcommon
