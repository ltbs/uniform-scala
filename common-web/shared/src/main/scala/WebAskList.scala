package ltbs.uniform
package common.web

import validation._
import concurrent._
import cats.implicits._

trait WebAskList[Html, A] extends Primatives[Html] {

  import WebAskList._

  def deleteConfirmationJourney(
    allRecords: List[A],
    deletionIndex: Int
  ): WebMonad[Html, Boolean] = true.pure[WebMonad[Html, *]]

  def menuPage: WebInteraction[Html, ListingTable[A], WebAskList.ListAction]
  implicit def codec: Codec[List[A]]

  def apply(
    key: String,
    askJourney: (Option[Int], List[A]) => WebMonad[Html,A],
    default: Option[List[A]],
    validation: Rule[List[A]],
    customContent: Map[String,(String, List[Any])],
  ): WebMonad[Html, List[A]] = new WebMonad[Html, List[A]] {

    val subRules = validation.subRules
    val (min, max) = subRules.collect {
      case Rule.MaxLength(h, _) => (0, h)
      case Rule.MinLength(l,_) => (l,Int.MaxValue)
      case Rule.LengthBetween(l,h) => (l,h)
    }.foldLeft((0, Int.MaxValue)){
      case ((al, ah),(bl, bh)) => (Math.max(al, bl), Math.min(ah, bh))
    }

    val elementValidation: Rule[A] = subRules.collect {
      case Rule.ForEachInList(r) => r
    }.combineAll

    def apply(pageIn: PageIn[Html])(implicit ec: ExecutionContext): Future[PageOut[Html,List[A]]] = {

      db.get[List[A]](List(s"${key}-zzdata")).flatMap { dataRead =>

        val data: List[A] = dataRead match {
          case Some(Right(d)) => d
          case _ => default.getOrElse(Nil)
        }

        val branchValidation = {
          import cats.data.Validated.Valid
          Rule.cond[ListAction]({
            case ListAction.Add if data.size >= max => false
            case _ => true
          }, "maxLength") followedBy {
            case ListAction.Continue => validation(data).map { _ => ListAction.Continue }
            case x => Valid(x)
          }
        }

        if (data.isEmpty && min > 0) {
          subjourneyWM(_.copy(leapAhead = false), Seq(key, "add"):_*)(for {
              r <- askJourney(None, data)
              _ <- db(List(s"${key}-zzdata")) = data :+ r
              _ <- db.deleteRecursive(List(key))
              _ <- goto[Unit](key)
            } yield (List.empty[A]))
        } else {
          menuPage(key, new ListingTable(data).some, None, branchValidation, customContent) flatMap {
            case ListAction.Continue =>
              data.pure[WebMonad[Html, *]]

            case ListAction.Add =>
              subjourneyWM(_.copy(leapAhead = false), Seq(key, "add"):_*)(for {
                r <- askJourney(None, data)
                _ <- db(List(s"${key}-zzdata")) = data :+ r
                _ <- db.deleteRecursive(List(key))
                _ <- goto[Unit](key)
              } yield (List.empty[A]))

            case ListAction.Delete(index) =>
              subjourneyWM(_.copy(leapAhead = false), Seq(key, "delete") :_ *)(for {
                confirm <- deleteConfirmationJourney(data, index)
                _ <- confirm match {
                  case true =>
                    db(List(s"${key}-zzdata")) = data.deleteAtIndex(index)
                  case false =>
                    ().pure[WebMonad[Html, *]]
                }
                _ <- db.deleteRecursive(List(key))
                _ <- goto[Unit](key)
              } yield (List.empty[A]))

            case ListAction.Edit(index) => {
              subjourneyWM(_.copy(leapAhead = false), Seq(key, "edit", index.toString) :_ *)(for {
                r <- askJourney(Some(index), data)
                _ <- db(List(s"${key}-zzdata")) = data.replaceAtIndex(index, r)
                _ <- db.deleteRecursive(List(key))
              _ <- goto[Unit](key)
              } yield (List.empty[A]))
            }
          }
        }
      }.apply(pageIn)
    }
  }
}

object WebAskList {

  sealed trait ListAction
  sealed trait ListActionRow extends ListAction
  sealed trait ListActionGeneral extends ListAction

  /** A representation of a list used for WebTell. This will typically
    * contain links as well as the data (for edit and delete)
    */
  final class ListingTable[A](val value: List[A]) extends AnyVal

  object ListAction {
    case class Delete(index: Int) extends ListActionRow
    case class Edit(index: Int) extends ListActionRow
    case object Continue extends ListActionGeneral
    case object Add extends ListActionGeneral
  }

  implicit def autoWebAskList[Html, A](
    implicit tellList: WebTell[Html, WebAskList.ListingTable[A]],
    codecIn: Codec[A],
    listActionCodec: Codec[WebAskList.ListAction],
    ff: WebAsk[Html, WebAskList.ListActionGeneral]
  ): WebAskList[Html, A] = new WebAskList[Html, A] {

    object Pos {
      def unapply(value: String): Option[Int] =
        Either.catchOnly[NumberFormatException](value.toInt).toOption
    }

    implicit def codec = new Codec[List[A]] {
      def decode(out: Input): Either[ErrorTree,List[A]] = {
        out.listSubtrees.sorted.traverse {
          index => codecIn.decode(out.atPath(index :: Nil))
        }
      }
      def encode(in: List[A]): Input = in.zipWithIndex.flatMap {
        case (v, i) => codecIn.encode(v).prefixWith(i.toString)
      }.toMap
    }

    def menuPage: WebInteraction[Html,WebAskList.ListingTable[A],WebAskList.ListAction] = new PostAndGetPage[Html, WebAskList.ListingTable[A], WebAskList.ListAction] {

      def codec: Codec[WebAskList.ListAction] = listActionCodec

      def getPage(
        key: List[String],
        tell: Option[WebAskList.ListingTable[A]],
        state: DB,
        existing: Input,
        breadcrumbs: Breadcrumbs,
        messages: UniformMessages[Html]
      )(implicit ec: ExecutionContext): Option[Html] = {
        val tellHtml = tell.flatMap(tellList.render(_, key.last, messages))
        ff.render(key, key, tellHtml, breadcrumbs, existing, ErrorTree.empty, messages)
      }

      def postPage(
        key: List[String],
        tell: Option[WebAskList.ListingTable[A]],
        state: DB,
        request: Input,
        errors: ErrorTree,
        breadcrumbs: Breadcrumbs,
        messages: UniformMessages[Html]
      )(implicit ec: ExecutionContext): Option[Html] = {
        val tellHtml = tell.flatMap(tellList.render(_, key.last, messages))
        ff.render(key, key, tellHtml, breadcrumbs, request, errors, messages)
      }

      override val customRouting: PartialFunction[List[String],WebAskList.ListAction] = {
        case "edit"   :: Pos(x) :: _   => WebAskList.ListAction.Edit(x)
        case "delete" :: Pos(x) :: Nil => WebAskList.ListAction.Delete(x)
      }

    }
  }

}
