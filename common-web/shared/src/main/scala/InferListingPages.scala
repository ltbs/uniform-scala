package ltbs.uniform
package common.web

import validation._
import cats.implicits._
import shapeless.Lazy

sealed trait ListAction
sealed trait ListActionRow extends ListAction
sealed trait ListActionGeneral extends ListAction

/** The data type returned form the branching part of a listing page. */
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

protected[web] object Pos {
  def unapply(value: String): Option[Int] =
    Either.catchOnly[NumberFormatException](value.toInt).toOption
}

/** Produces pages for `List[A]` where a user can enter multiple items
  * of a given datatype via a central page that enumerates the items
  * already, allows them to add new items and edit or delete existing ones.
  *
  * There are two ways you can produce a listing - the first works by
  * infering the page for `A` automatically. To use this approach
  * simply use an `ask[List[A]]` in your journey and it will pick up
  * and use the add/edit part from inference, falling back to
  * `InferFormField` if necessary. 
  * 
  * The second approach works by explicitly supplying a subjourney and
  * interpreting this into a [[WebMonadConstructor]], by using the
  * `listingPageWM` method. 
  */
trait InferListingPages[Html] {
  this: GenericWebInterpreter[Html] =>

  def blankTell: Html

  implicit def listingPage[A](implicit
    wmca: Lazy[WMC[A]],
    codec: Codec[List[A]],
    listingTell: ListingTell[Html, A],
    wmcbranchffg: FormField[ListActionGeneral, Html],
    wmcbranchffa: FormField[ListAction, Html]
  ): WMC[List[A]] = listingPageWM(
    addEditJourney = {(existing: List[A], edit: Option[Int], messages: UniformMessages[Html], validation) =>
      wmca.value.apply(
        id = if (edit.isDefined) "edit" else "add", 
        tell = blankTell,
        defaultIn = edit.map(existing.apply),
        validationIn = validation,
        messages = messages
      )
    },
    useSubjourneys = false
  )

  /** Allows creation of a listing page that uses an interpreted
    * subjourney in place of the implicit single-page form used for
    * adding and editing. 
    * 
    * {{{
    *   case class MyClass(a: Boolean, b: String)
    * 
    *   type SubjourneyTell = NilTypes
    *   type SubjourneyAsk = Boolean :: String :: NilTypes
    * 
    *   def myclassSubjourney[F[_]: cats.Monad]( 
    *     existing: List[MyClass], 
    *     editIndex: Option[Int], 
    *     messages: UniformMessages[Html]
    *   )(
    *     int: Language[F, SubjourneyTell, SubjourneyAsk]
    *   ): F[MyClass] = ???
    * 
    *   implicit def myClassListing(
    *     implicit request: Request[AnyContent]
    *   ) = {
    *     interpreter.listingPageWM[MyClass](
    *       subjourney[interpreter.WM](_,_,_)(
    *         create[SubjourneyTell, SubjourneyAsk](interpreter.messages(request))
    *       )
    *     )
    *   }
    * }}}
    * 
    * @param addEditJourney the interpreted journey used for adding
    *        and editing. Accepts the existing list of entries, an
    *        index for editing (None if adding), and messages. 
    * @param deleteJourney an optional journey to be invoked when
    *        deleting, must return a boolean
    * @param customOrdering allows the entries to be ordered
    *        differently in the listing. By default they will be in
    *        the order added. 
    */
  def listingPageWM[A](
    addEditJourney: (List[A], Option[Int], UniformMessages[Html], Rule[A]) => WM[A],
    deleteJourney: (List[A], Int) => WM[Boolean] = {(_: List[A], _: Int) => true.pure[WM]},
    customOrdering: Option[cats.Order[A]] = None,
    useSubjourneys: Boolean = true
  )(implicit
    wmcbranchffg: FormField[ListActionGeneral, Html],
    wmcbranchffa: FormField[ListAction, Html],    
    codec: Codec[List[A]],
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
      override val customRouting: PartialFunction[List[String],ListAction] = {
        case "edit" :: Pos(x) :: Nil   => ListAction.Edit(x)
        case "delete" :: Pos(x) :: Nil => ListAction.Delete(x)
      }
    }

    def apply(
      id: String,
      tell: Html,
      defaultIn: Option[List[A]],
      validationIn: Rule[List[A]],
      messages: UniformMessages[Html]
    ): WM[List[A]] = {

      val subRules = validationIn.subRules
      val (min, max) = subRules.collect {
        case Rule.MaxLength(h, _) => (0, h)
        case Rule.MinLength(l,_) => (l,Int.MaxValue)
        case Rule.LengthBetween(l,h) => (l,h)          
      }.foldLeft((0, Int.MaxValue)){
        case ((al, ah),(bl, bh)) => (Math.max(al, bl), Math.min(ah, bh))
      }

      def subJourney[S](id: Seq[String])(sub: => WM[S]): WM[S] =
        if (useSubjourneys)
          genericSubJourney[S](id, c => c.copy(leapAhead = false))(sub)
        else
          genericSubJourney[S](id, c => c.copy(leapAhead = false))(sub)

      getConfig().flatMap { config: JourneyConfig =>

        db.get[List[A]](List(s"${id}-zzdata")).flatMap { dataRead =>

          val data: List[A] = dataRead match {
            case Some(Right(d)) => d
            case _ => defaultIn.getOrElse(Nil)
          }

          val orderWithIndex: Ordering[(A, Int)] = {
            new cats.Order[(A, Int)] {
              def compare(a: (A, Int), b: (A, Int)): Int = customOrdering match {
                case Some(o) => o.compare(a._1, b._1)
                case None => a._2 compareTo b._2
              }
            }
          }.toOrdering

          val indexedRows = data.zipWithIndex.
            sorted(orderWithIndex).
            map { case (v, index) =>
              val editLink = s"$id/edit/$index" + (if (useSubjourneys) "/" else "")
              val deleteLink = s"$id/delete/$index/"
              ListingTellRow(v, editLink, deleteLink)
            }

          val validation = {
            import cats.data.Validated.Valid
            Rule.cond[ListAction]({
              case ListAction.Add if data.size >= max => false
              case _ => true
            }, "maxLength") followedBy {
              case ListAction.Continue => validationIn(data).map { _ => ListAction.Continue }
              case x => Valid(x)
            }
          }

          val elementValidation: Rule[A] = subRules.collect {
            case Rule.ForEachInList(r) => r
          }.combineAll

          (if (config.askFirstListItem && data.isEmpty && min > 0) {
            (ListAction.Add: ListAction).pure[WM]
          } else {
            wmcbranch(id, listingRowHtml(indexedRows, messages), None, validation, messages): WM[ListAction]
          }) flatMap {
            case ListAction.Continue =>
              data.pure[WM]

            case ListAction.Add =>
              subJourney(Seq(id, "add"))(for {
                r <- addEditJourney(data, None, messages, elementValidation)
                _ <- db(List(s"${id}-zzdata")) = data :+ r
                _ <- db.deleteRecursive(List(id))
                _ <- goto[Unit](id)
              } yield (List.empty[A]))

            case ListAction.Delete(index) =>
              subJourney(Seq(id, "delete"))(for {
                confirm <- deleteJourney(data, index)
                _ <- confirm match {
                  case true =>
                    db(List(s"${id}-zzdata")) = data.deleteAtIndex(index)
                  case false =>
                    ().pure[WM]
                }
                _ <- db.deleteRecursive(List(id))
                _ <- goto[Unit](id)
              } yield (List.empty[A]))

            case ListAction.Edit(index) => {
              subJourney(Seq(id, "edit", index.toString))(for {
                r <- addEditJourney(data, Some(index), messages, elementValidation)
                _ <- db(List(s"${id}-zzdata")) = data.replaceAtIndex(index, r)
                _ <- db.deleteRecursive(List(id))
                _ <- goto[Unit](id)
              } yield (List.empty[A]))
            }
          }
        }
      }
    }
  }
}
