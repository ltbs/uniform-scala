package ltbs.uniform.sampleprograms

import cats.implicits._
import ltbs.uniform._
import enumeratum._
import java.time.{LocalDate => Date}
import scala.language.higherKinds

object BeardTaxUF4 {

  type BeardLength = (Int, Int)

  case class MemberOfPublic(
    forename: String,
    surname: String,
    age: Date
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

  def costOfBeard(beardStyle: BeardStyle, length: BeardLength): Int =
    beardStyle match {
      case BeardStyle.SoulPatch => length._2 / 10
      case _                    => length._1 + (length._2 - length._1) / 2
    }

  implicit class RichApplicative[F[_]: cats.Applicative, A](inner: F[A]) {
    def unless(in: Boolean): F[Option[A]] =
      if (in) inner.map(_.some) else none[A].pure[F]

    def when(in: Boolean): F[Option[A]] = unless(!in)    
  }

  implicit class RichApplicativeMonoid[F[_]: cats.Applicative, A: cats.Monoid](inner: F[A]) {
    def emptyUnless(in: Boolean): F[A] =
      if (in) inner else cats.Monoid[A].empty.pure[F]

    def emptyWhen(in: Boolean): F[A] = emptyUnless(!in)    
  }

  implicit class RichMonadMonoid[F[_]: cats.Monad, A: cats.Monoid](inner: F[A]) {
    def emptyUnless(in: F[Boolean]): F[A] =
      in flatMap {
        case false => cats.Monoid[A].empty.pure[F]
        case true => inner
      }

    def emptyWhen(in: F[Boolean]): F[A] = emptyUnless(in.map(!_))    
  }
  
  def program[F[_]: cats.Monad](interpreter: Language[F,  // F must be a monad
    Unit :: HNil, // we never 'tell' the user anything in this program - we just ask
    Unit :: Option[MemberOfPublic] :: BeardStyle :: BeardLength :: HNil // data types we're interested in
  ]): F[Int] = {
    import interpreter._
    for {
      memberOfPublic <- ask[Option[MemberOfPublic]](
        id = "is-public",
        validation = ValidationRule(
          {x: Option[MemberOfPublic] => x.map{_.age.isBefore(Date.now)}.getOrElse(true)} -> ErrorMsg("born-in-future")
        )
      )
      beardStyle     <- ask[BeardStyle](
        id = "beard-style",
        customContent = Map(
          "beard-style.heading" -> {memberOfPublic match {
            case Some(MemberOfPublic(forename,_,_)) =>
              ("beard-style.heading.menacing", List(forename))
            case None =>
              ("beard-style.heading.sycophantic",Nil)
          }}
        )
      )
      beardLength    <- ask[BeardLength](
        id = "beard-length-mm",
        validation = ValidationRule(
          {x: (Int,Int) => x._1 > x._2 } -> ErrorMsg("lower-exceeds-higher")
        )
      ) emptyUnless memberOfPublic.isDefined
    } yield costOfBeard(beardStyle, beardLength)
  }

}
