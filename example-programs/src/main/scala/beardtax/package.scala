package ltbs.uniform
package examples

import cats.Monad
import izumi.reflect.TagK
import scala.language.higherKinds
import validation.Rule
import cats.implicits._

package object beardtax {

  type BeardLength = (Int,Int)

  def beardProgram[F[_] : Monad : TagK](
    hod: Hod[F]
  ) = for {
    beardHeight <- (
      ask[Int]("chin-height"),
      ask[Int]("sideburn-height")
    ).mapN{case (a,b) => a + b}
    _ <- convertWithKey("rec-height")(hod.recordBeardHeight(beardHeight))
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
    cost           <- convert(hod.costOfBeard(beardStyle, beardLength))  // Future ~> WM
                                                                         // Future[A] => WM[A]
                                                                         // Converter[Future, WM, A]
  } yield cost

}
