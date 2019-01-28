package ltbs.uniform.sampleprograms

import cats.implicits._
import org.atnos.eff._
import ltbs.uniform._
import enumeratum._

object BeardTax { 

  type Name        = (String, String)
  type BeardLength = (Int, Int)

  case class MemberOfPublic(
    forename: String,
    surname: String,
    age: java.time.LocalDate
  )


  sealed trait BeardStyle extends EnumEntry
  object BeardStyle extends Enum[BeardStyle] {
    val values = findValues
    case object Goatee           extends BeardStyle
    case object Horseshoe        extends BeardStyle
    case object Gunslinger       extends BeardStyle
    case object MuttonChops      extends BeardStyle
    case object SoulPatch        extends BeardStyle
    case object LaughingCavalier extends BeardStyle            
  }

  type TestProgramStack = Fx3[
    UniformAsk[Option[MemberOfPublic],?],
    UniformAsk[BeardStyle,?],
    UniformAsk[BeardLength,?]
  ]

  def costOfBeard(length: BeardLength): Int =
    length._1 + (length._2 - length._1) / 2

  def program[R
      : _uniform[Option[MemberOfPublic],?]
      : _uniform[BeardStyle,?]
      : _uniform[BeardLength,?]
  ]: Eff[R, Int] =
    for {
      memberOfPublic <- uask[R, Option[MemberOfPublic]]("is-public")
      beardStyle     <- uask[R, BeardStyle]("beard-style")            
      beardLength    <- uask[R, BeardLength]("beard-length-mm", validation = {
        case a@(l,h) => if (l > h) "lower-exceeds-higher".invalid else a.valid
      }) emptyUnless (memberOfPublic.isDefined)
    } yield costOfBeard(beardLength)

}
