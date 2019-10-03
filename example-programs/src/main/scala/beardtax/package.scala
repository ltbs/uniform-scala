package ltbs.uniform
package examples

import cats.Monad
import cats.implicits._
import cats.data.NonEmptyList

package object beardtax {

  type BeardLength = (Int,Int)
  type TellTypes = NilTypes
  type AskTypes = Option[MemberOfPublic] :: BeardStyle :: BeardLength :: NilTypes

  def beardProgram[F[_] : Monad](
    interpreter: Language[F, TellTypes, AskTypes],
    hod: Hod[F]
  ): F[Int] = {
    import interpreter._
    for {
      memberOfPublic <- ask[Option[MemberOfPublic]]("is-public")
      beardStyle     <- ask[BeardStyle]("beard-style", customContent = Map(
        "beard-style" -> {memberOfPublic match {
          case None                              => ("beard-style-sycophantic", Nil)
          case Some(MemberOfPublic(_, sname, _)) => ("beard-style-menacing", List(sname))
        }}
      ))
      beardLength    <- ask[BeardLength]("beard-length-mm", validation = List(List(
        Rule.fromPred(x => x._1 <= x._2, (ErrorMsg("lower.less.than.higher"), NonEmptyList.one(Nil)))
      ))) emptyUnless memberOfPublic.isDefined
      cost           <- hod.costOfBeard(beardStyle, beardLength)
    } yield cost
  }

}
