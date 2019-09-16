package ltbs.uniform
package common.web

import cats.implicits._
import cats.Monoid

sealed trait ListAction
sealed trait ListActionRow extends ListAction
sealed trait ListActionGeneral extends ListAction

object ListAction {
  case class Delete(index: Int) extends ListActionRow
  case class Edit(index: Int) extends ListActionRow
  case object Continue extends ListActionGeneral
  case object Add extends ListActionGeneral
}

trait ListingRowHtml[Html, A] {
  def apply(index: Int, value: A, editLink: String, deleteLink: String, messages: UniformMessages[Html]): Html
}

object Pos {
  def unapply(value: String): Option[Int] =
    Either.catchOnly[NumberFormatException](value.toInt).toOption
}

trait ListingGenerator[Html] {
  this: GenericWebInterpreter[Html] =>

  def genericListingPage(
    rows: List[(Html, Int)]
  ): Html

  def listingPage[A](implicit
    wmcbranch: WMC[ListAction],
    wmca: WMC[A],
    mon: Monoid[Html],
    codec: Codec[List[A]],
    listingRowHtml: ListingRowHtml[Html, A]
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

        val listingPage = genericListingPage(data.map(listingRowHtml(0, _, "", "", messages)).zipWithIndex)

        {wmcbranch(id, listingPage, None, Nil, messages): WM[ListAction]} flatMap {
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
    addEditJourney: (List[A], Option[Int]) => WM[A],
    deleteJourney: (List[A], Int) => WM[Boolean] = {(_: List[A], _: Int) => true.pure[WM]},
    customOrdering: Option[cats.Order[A]] = None
  )(implicit
    wmcbranchffg: FormField[ListActionGeneral, Html],
    wmcbranchffa: FormField[ListAction, Html],    
    codec: Codec[List[A]],
    mon: Monoid[Html],
    listingRowHtml: ListingRowHtml[Html, A]
  ) = new WMC[List[A]] {

    val wmcbranchff = new FormField[ListAction, Html] {
      def decode(out: Input): Either[ErrorTree,common.web.ListAction] =
        wmcbranchffa.decode(out)

      def encode(in: common.web.ListAction): Input =
        wmcbranchffa.encode(in)

      def render(
        key: List[String],
        breadcrumbs: common.web.Breadcrumbs,
        data: Input,
        errors: ErrorTree,
        messages: UniformMessages[Html]
      ): Html =
        wmcbranchffg.render(key, breadcrumbs, data, errors, messages)
    }

    val wmcbranch = new SimplePostAndGetPage[ListAction, Html](
      wmcbranchff
    ) {
      override val customRouting = {
        case "edit" :: Pos(x) :: Nil   => ListAction.Edit(x)
        case "delete" :: Pos(x) :: Nil => ListAction.Delete(x)
      }
    }

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
          map{case (v, index) =>

            val editLink = s"$id/edit/$index"
            val deleteLink = s"$id/delete/$index"

            (listingRowHtml(index, v, editLink, deleteLink, messages), index)
          }

        {wmcbranch(id, genericListingPage(indexedRows), None, Nil, messages): WM[ListAction]} flatMap {
          case ListAction.Continue =>
            data.pure[WM]

          case ListAction.Add =>
            for {
              newRecord <- genericSubJourney(Seq(id, "add"))(addEditJourney(data, None))
              _         <- db(List(s"${id}-zzdata")) = data :+ newRecord
              _         <- db.deleteRecursive(List(id)) >> db.delete(List(id))
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
              newRecord <- genericSubJourney(Seq(id, "edit", index.toString))(addEditJourney(data, Some(index)))
              _         <- db(List(s"${id}-zzdata")) = data.replaceAtIndex(index, newRecord)
              _         <- db.deleteRecursive(List(id, "edit")) >> db.delete(List(id))
              x         <- goto[List[A]](id)
            } yield (x)

        }
      }
    }
  }
}
