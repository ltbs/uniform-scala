package ltbs.uniform
package examples

import cats.Monad
import izumi.reflect.TagK
import scala.language.higherKinds
import validation.Rule

package object beardtax {

  type BeardLength = (Int,Int)

  def beardProgram[F[_] : Monad : TagK](
    hod: Hod[F]
  ) = for {
      memberOfPublic <- ask[Option[MemberOfPublic]]("is-public")
      beardStyle     <- ask[BeardStyle]("beard-style", customContent = Map(
        "beard-style" -> {memberOfPublic match {
          case None                              => ("beard-style-sycophantic", Nil)
          case Some(MemberOfPublic(_, sname, _)) => ("beard-style-menacing", List(sname))
        }}
      ))
      beardLength    <- ask[BeardLength]("beard-length-mm", validation = 
        Rule.condAtPath("_2")(x => x._1 <= x._2, "lower.less.than.higher")
      ) emptyUnless (memberOfPublic.isDefined    )
      cost           <- convert(hod.costOfBeard(beardStyle, beardLength))

    } yield cost

}
