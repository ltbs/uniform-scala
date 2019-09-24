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

case class ListingTellRow[A](value: A, editLink: String, deleteLink: String)

/** A representation of how to render a listing of a type of object */
trait ListingTell[Html, A] {
  def apply(rows: List[ListingTellRow[A]], messages: UniformMessages[Html]): Html
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
    wmca: WMC[A],
    mon: Monoid[Html],
    codec: Codec[List[A]],
    listingTell: ListingTell[Html, A],
    wmcbranchffg: FormField[ListActionGeneral, Html],
    wmcbranchffa: FormField[ListAction, Html]
  ): WMC[List[A]] = listingPageWM(
    addEditJourney = {(existing: List[A], edit: Option[Int], messages: UniformMessages[Html]) =>
      wmca.apply(
        id = if (edit.isDefined) "edit" else "add", 
        tell = Monoid[Html].empty,
        defaultIn = edit.map(existing.apply),
        validationIn = List.empty,
        messages = messages
      )
    },
    useSubjourneys = false
  )

  def listingPageWM[A](
    addEditJourney: (List[A], Option[Int], UniformMessages[Html]) => WM[A],
    deleteJourney: (List[A], Int) => WM[Boolean] = {(_: List[A], _: Int) => true.pure[WM]},
    customOrdering: Option[cats.Order[A]] = None,
    useSubjourneys: Boolean = true
  )(implicit
    wmcbranchffg: FormField[ListActionGeneral, Html],
    wmcbranchffa: FormField[ListAction, Html],    
    codec: Codec[List[A]],
    mon: Monoid[Html],
    listingRowHtml: ListingTell[Html, A]
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

      def subJourney[S](id: Seq[String])(sub: => WM[S]): WM[S] =
        if (useSubjourneys)
          genericSubJourney[S](id)(sub)
        else
          genericSubJourney[S](id.init)(sub)

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
            ListingTellRow(v, editLink, deleteLink)
          }

        {wmcbranch(id, listingRowHtml(indexedRows, messages), None, Nil, messages): WM[ListAction]} flatMap {
          case ListAction.Continue =>
            data.pure[WM]

          case ListAction.Add =>
            subJourney(Seq(id, "add"))( for {
              r <- addEditJourney(data, None, messages)
              _ <- db(List(s"${id}-zzdata")) = data :+ r
              _ <- db.deleteRecursive(List(id))
            } yield (List.empty[A]))

          case ListAction.Delete(index) =>
            subJourney(Seq(id, "delete"))( for {
              confirm <- deleteJourney(data,index)
              _       <- confirm match {
                case true =>
                  db(List(s"${id}-zzdata")) = data.deleteAtIndex(index)
                case false =>
                  ().pure[WM]
              }
              _ <- db.deleteRecursive(List(id))
            } yield (List.empty[A]))

          case ListAction.Edit(index) =>
            subJourney(Seq(id, "edit"))( for {
              r <- addEditJourney(data, Some(index), messages)
              _ <- db(List(s"${id}-zzdata")) = data :+ r
              _ <- db.deleteRecursive(List(id))
            } yield (List.empty[A]))
        }
      }
    }
  }
}
