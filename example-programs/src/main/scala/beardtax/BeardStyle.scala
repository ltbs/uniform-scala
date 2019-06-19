package ltbs.uniform.examples.beardtax

sealed trait BeardStyle

object BeardStyle {
  final case object Goatee           extends BeardStyle
  final case object Horseshoe        extends BeardStyle
  final case object MuttonChops      extends BeardStyle
  final case object SoulPatch        extends BeardStyle
  final case object LaughingCavalier extends BeardStyle

  final case class Gunslinger(
    gramsOfWax: Int
  ) extends BeardStyle

}
