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

  val interpreter = this

  def genericListingPage(
    rows: List[(Html, Int)]
  ): Html

  implicit class RichList[A](value: List[A]) {
    def deleteAtIndex(i: Int): List[A] = ???
    def replaceAtIndex(i: Int, a: A): List[A] = ???
  }

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
      val data: List[A] = defaultIn.getOrElse(Nil)

      {wmcbranch(id, genericListingPage(data.map(row).zipWithIndex), None, Nil, messages): WM[ListAction]} flatMap {
        case ListAction.Continue => data.pure[WM]
        case ListAction.Add =>
          for {
            newRecord <- wmca.apply(s"${id}-add", Monoid[Html].empty, None, Nil, messages): WM[A]
            _ <- db(List(s"${id}-zzdata")) = data :+ newRecord
            _ <- db.delete(List(s"${id}-add"))
            x <- goto[List[A]](id)
          } yield (x)
        case ListAction.Delete(index) =>
          {db(List(s"${id}-zzdata")) = data.deleteAtIndex(index)} >> goto[List[A]](id)

        case ListAction.Edit(index) =>
          val editData: A = data(index)
          for {
            newRecord <- wmca.apply(s"${id}-edit", Monoid[Html].empty, Some(editData), Nil, messages): WM[A]
            _ <- db(List(s"${id}-zzdata")) = data.replaceAtIndex(index, newRecord)
            _ <- db.delete(List(s"${id}-edit"))
            x <- goto[List[A]](id)
          } yield (x)
          
      }
    }

  }
}
