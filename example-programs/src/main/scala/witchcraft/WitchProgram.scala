package ltbs.uniform
package examples

import cats.Monad
import cats.implicits._

package object witchcraft {

  type TellTypes = NilTypes
  type AskTypes = Accused :: List[Evidence] :: List[Familiar] :: NilTypes

  def witchProgram[F[_] : Monad](
    i: Language[F, TellTypes, AskTypes]
  ): F[WitchReport] = for {
    f <- i.ask[List[Familiar]]("familiars")
    e <- i.ask[List[Evidence]]("evidence")
    a <- i.ask[Accused]("accused")
  } yield WitchReport(a,e,f)

}
