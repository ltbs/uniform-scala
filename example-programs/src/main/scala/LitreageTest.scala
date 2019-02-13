package ltbs.uniform.sampleprograms

import cats.implicits._
import org.atnos.eff._
import cats.data.Validated
import ltbs.uniform._

object LitreageTest {

  type Litres = Option[(Long,Long)]

  implicit class RichVal[A](private val a:A) extends AnyVal {
    def check[B](pred: A => Boolean, error: B): Validated[B,A] =
      Validated.cond(pred(a), a, error)
    def checkEither[B](pred: A => Boolean, error: B): Either[B,A] =
      Either.cond(pred(a), a, error)
  }

  type TestProgramStack = Fx2[Uniform[Unit,Litres,?], Uniform[Unit,Boolean,?]]

  def program[R
      : _uniformCore
      : _uniformAsk[Litres,?]
      : _uniformAsk[Boolean,?]
  ]: Eff[R, String] = for {
    n <- ask[Litres]("litresProduced")
           .validating("lower cannot be more than higher", {
              case Some((l,h)) => l <= h
              case None        => true
           })
    s <- ask[Boolean]("imports")
    t <- ask[Boolean]("copacksForOthers")
    i <- ask[Litres]("copackedByOtherUk")
  } yield (s"$s AND $n")

}
