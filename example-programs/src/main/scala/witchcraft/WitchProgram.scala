package ltbs.uniform
package examples

import cats.Monad
import cats.implicits._
import scala.language.higherKinds

package object witchcraft {

  type BeardLength = (Int,Int)
  type TellTypes = NilTypes
  type AskTypes = Accused :: List[Evidence] :: List[Familiar] :: NilTypes

  def witchProgram[F[_] : Monad](
    i: Language[F, TellTypes, AskTypes]
  ): F[WitchReport] = (
    i.ask[Accused]("accused"),
    i.ask[List[Evidence]]("evidence"),
    i.ask[List[Familiar]]("familiars")
  ).mapN(WitchReport)

}
