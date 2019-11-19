package ltbs.uniform
package examples

import cats.Monad
import cats.implicits._
import scala.language.higherKinds
import validation._

package object beardtax {

  type BeardLength = (Int,Int)
  type TellTypes = NilTypes
  type AskTypes = Int :: MemberOfPublic :: BeardLength :: NilTypes

  def beardProgram[F[_] : Monad](
    interpreter: Language[F, TellTypes, AskTypes],
    hod: Hod[F]
  ): F[Int] = {
    import interpreter._
    for {
      a <- ask[Int]("a")
      b <- ask[BeardLength]("b")
      c <- ask[Int]("c")
      d <- ask[Int]("d")                  
    } yield a + b._1 + b._2 + c + d
  }

}
