package ltbs.uniform
package common.web

import cats.implicits._
import cats.Monoid

sealed trait ListAction
object ListAction {
  case object Continue extends ListAction
  case object Add extends ListAction
  case class Delete(index: Int) extends ListAction
  case class Edit(index: Int) extends ListAction
}

trait ListingGenerator[Html] {
  this: GenericWebInterpreter[Html] =>

  def genericListingPage(
    rows: List[(Html, Int)]
  ): Html

  def listingPage[A](
    row: A => Html
  )(implicit
    wmcbranch: WMC[ListAction],
    wmca: WMC[A],
    mon: Monoid[Html],
    codec: Codec[List[A]]
  ) = new WMC[List[A]] {

    def apply(
      id: String,
      tell: Html,
      defaultIn: Option[List[A]],
      validationIn: List[List[Rule[List[A]]]],
      messages: UniformMessages[Html]
    ): WM[List[A]] = {

      db.get[List[A]](List(s"${id}-zzdata")).flatMap{ dataRead =>

        val data: List[A] = dataRead match {
          case Some(Right(d)) => d
          case _ => defaultIn.getOrElse(Nil)
        }

        {wmcbranch(id, genericListingPage(data.map(row).zipWithIndex), None, Nil, messages): WM[ListAction]} flatMap {
          case ListAction.Continue => data.pure[WM]

          case ListAction.Add =>
            for {
              newRecord <- wmca.apply(s"${id}-add", Monoid[Html].empty, None, Nil, messages): WM[A]
              _ <- db(List(s"${id}-zzdata")) = data :+ newRecord
              _ <- db.delete(List(s"${id}-add")) >> db.delete(List(id))
              x <- goto[List[A]](id)
            } yield (x)

          case ListAction.Delete(index) =>
            {db(List(s"${id}-zzdata")) = data.deleteAtIndex(index)} >> db.delete(List(id)) >> goto[List[A]](id)

          case ListAction.Edit(index) =>
            val editData: A = data(index)
            for {
              newRecord <- wmca.apply(s"${id}-edit", Monoid[Html].empty, Some(editData), Nil, messages): WM[A]
              _ <- db(List(s"${id}-zzdata")) = data.replaceAtIndex(index, newRecord)
              _ <- db.delete(List(s"${id}-edit")) >> db.delete(List(id))
              x <- goto[List[A]](id)
            } yield (x)

        }
      }
    }

  }
}
