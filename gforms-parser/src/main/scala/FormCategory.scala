package ltbs.uniform.gformsparser

import enumeratum._

sealed trait FormCategory extends EnumEntry with EnumEntry.Uncapitalised
object FormCategory extends Enum[FormCategory] {
  case object HmrcReturnForm extends FormCategory
  case object HmrcClaimForm extends FormCategory
  lazy val values = findValues
}
