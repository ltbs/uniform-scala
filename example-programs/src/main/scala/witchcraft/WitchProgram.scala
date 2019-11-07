package ltbs.uniform
package examples

import scala.language.higherKinds

import validation._
import cats.Monad
import cats.implicits._

package object witchcraft {

  type TellTypes = NilTypes
  type AskTypes = Accused :: List[Evidence] :: List[Familiar] :: NilTypes

  def witchProgram[F[_] : Monad](
    i: Language[F, TellTypes, AskTypes]
  ): F[WitchReport] = for {
    f <- i.ask[List[Familiar]]("familiars", validation = Rule.lengthBetween(1, 3))
    e <- i.ask[List[Evidence]]("evidence", validation =
      Rule.forEachInList(Rule.cond({_ != Evidence.HasWartOnNose}, "dont-care-about-warts")))
    a <- i.ask[Accused]("accused")
  } yield WitchReport(a,e,f)

}
