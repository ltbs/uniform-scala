package ltbs.uniform

import cats.data.Validated

package object webmonad {

  type Encoded = String
  type DB = Map[String,Encoded]
  type ValidationError = String
  type ValidatedData[A] = Option[Validated[ValidationError, A]]

}
