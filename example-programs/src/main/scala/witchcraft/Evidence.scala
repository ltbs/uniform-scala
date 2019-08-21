package ltbs.uniform
package examples.witchcraft

sealed trait Evidence

object Evidence {
  case object MySpouseFindsThemAttractive extends Evidence
  case object MyCropsFailed extends Evidence
  case object NotACatholic extends Evidence
  case object DidntDieWhenMurdered extends Evidence
  case object LivesAlone extends Evidence
  case object OutdidMeAtSomething extends Evidence
  case object Metamorphoses extends Evidence
  case object HasWartOnNose extends Evidence
  case object HealsUsingMedicineNotPrayer extends Evidence
  case object LowSocialStanding extends Evidence
}
