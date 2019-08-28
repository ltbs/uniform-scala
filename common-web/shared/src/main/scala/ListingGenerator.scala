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
              _ <- db.deleteRecursive(List(s"${id}-add")) >> db.delete(List(id))
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

  def listingPageWM[A](
    row: (A, Int) => Html,
    addEditJourney: (List[A], Option[Int]) => WM[A],
    deleteJourney: (List[A], Int) => WM[Boolean] = {(_: List[A], _: Int) => true.pure[WM]},
    customOrdering: Option[cats.Order[A]] = None
  )(implicit
    wmcbranch: WMC[ListAction],
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

        val orderWithIndex: Ordering[(A, Int)] = {new cats.Order[(A, Int)] {
          def compare(a: (A, Int), b: (A, Int)): Int = customOrdering match {
            case Some(o) => o.compare(a._1, b._1)
            case None    => a._2 compareTo b._2
          }
        }}.toOrdering

        val indexedRows = data.zipWithIndex.
          sorted(orderWithIndex).
          map{x => (row(x._1, x._2), x._2)}

        {wmcbranch(id, genericListingPage(indexedRows), None, Nil, messages): WM[ListAction]} flatMap {
          case ListAction.Continue =>
            data.pure[WM]

          case ListAction.Add =>
            for {
              newRecord <- genericSubJourney(s"${id}-add")(addEditJourney(data, None))
              _         <- db(List(s"${id}-zzdata")) = data :+ newRecord
              _         <- db.deleteRecursive(List(s"${id}-add")) >> db.delete(List(id))
              x         <- goto[List[A]](id)
            } yield (x)

          case ListAction.Delete(index) =>
            val action = deleteJourney(data,index) flatMap {
            case true =>
                db(List(s"${id}-zzdata")) = data.deleteAtIndex(index)
            case false =>
                ().pure[WM]
            }
            action >> db.delete(List(id)) >> goto[List[A]](id)

          case ListAction.Edit(index) =>
            for {
              newRecord <- addEditJourney(data, Some(index))
              _         <- db(List(s"${id}-zzdata")) = data.replaceAtIndex(index, newRecord)
              _         <- db.delete(List(s"${id}-edit")) >> db.delete(List(id))
              x         <- goto[List[A]](id)
            } yield (x)

        }
      }
    }

  }

}
