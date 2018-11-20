package ltbs.uniform.sampleprograms

import cats.implicits._
import org.atnos.eff._
import cats.data.Validated
import ltbs.uniform._

object LitreageTest {

  type Litres = (Long,Long)

  implicit class RichVal[A](private val a:A) extends AnyVal {
    def check[B](pred: A => Boolean, error: B): Validated[B,A] =
      Validated.cond(pred(a), a, error)
    def checkEither[B](pred: A => Boolean, error: B): Either[B,A] =
      Either.cond(pred(a), a, error)
  }

  type TestProgramStack = Fx2[UniformAsk[Litres,?], UniformAsk[Boolean,?]]

  def program[R : _uniform[Litres,?] : _uniform[Boolean,?]]: Eff[R, String] = for {
    n <- uask[R, Litres]("litresProduced", validation = {case a@(l,h) => if (l > h) "lower cannot be more than higher".invalid else a.valid})
    s <- uask[R, Boolean]("imports")
    t <- uask[R, Boolean]("copacksForOthers")
    i <- uask[R, Litres]("copackedByOtherUk")
  } yield (s"$s AND $n")

}
