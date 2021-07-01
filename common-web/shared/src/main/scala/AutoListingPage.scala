package ltbs.uniform
package common.web

import scala.concurrent._
import cats.implicits._
import validation.Rule
import scala.language.higherKinds

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

protected[web] object Pos {
  def unapply(value: String): Option[Int] =
    Either.catchOnly[NumberFormatException](value.toInt).toOption
}

case class ListingRow[Html](
  index: Int,
  content: Option[Html],
  editLink: String,
  deleteLink: String
)

trait AutoListingPage[Html] extends InferFormFields[Html] with Primatives[Html] {

  override def blockCollections[X[_] <: Traversable[_], A]: FormField[Html, X[A]] = ???
  override def blockCollections2[X[_] <: Traversable[_], A]: FormField[Html, X[A]] = ???

  implicit def inferListingPage[A](
    implicit ff: FormField[Html, A],
    teller: GenericWebTell[Html, A]
  ) = new WebInteraction[Html, Unit, List[A]] {
    def apply(
      id: String,
      tell: Option[Unit],
      defaultIn: Option[List[A]],
      validationIn: Rule[List[A]],
      customContent: Map[String,(String, List[Any])]
    ): WebMonad[Html,List[A]] = {
      val askList = fromTell(teller, ff)
      val interaction = new StandardTellAndAskForm(teller, ff)
      askList.apply(
        id,
        {(index, existing) => interaction.apply("entry", None, index.map(existing), Rule.alwaysPass, Map.empty) },
        defaultIn,
        validationIn,
        customContent
      )
    }
  }

  def renderListPage[A](
    pageKey: List[String],
    breadcrumbs: Breadcrumbs,
    existingEntries: List[ListingRow[Html]],
    data: Input,
    errors: ErrorTree,
    messages: UniformMessages[Html],
    validation: Rule[List[A]]
  ): Html


  protected def listingForm[A](
    key: String, 
    existingEntries: List[A],
    validation: Rule[List[A]],
    tell: GenericWebTell[Html, A],
    customOrdering: Option[Ordering[A]] = None    
  ) = {
    val ff = new FormField[Html, ListAction] {

      val orderWithIndex: Ordering[(A, Int)] = {
        new cats.Order[(A, Int)] {
          def compare(a: (A, Int), b: (A, Int)): Int = customOrdering match {
            case Some(o) => o.compare(a._1, b._1)
            case None => a._2 compareTo b._2
          }
        }
      }.toOrdering

      def decode(out: Input): Either[ErrorTree,ListAction] = {

        def readInt[Z](prefix: String, f: Int => Z)(data: Input): Either[ErrorTree, Z] = {
          Either.fromOption(
            data.valueAt(prefix),
            new NumberFormatException
          ).flatMap {
            case x :: Nil => Either.catchOnly[NumberFormatException](x.toInt)
            case _ => (new NumberFormatException).asLeft[Int]
          }.bimap(
            _ => ErrorMsg("invalid").toTree,
            f
          )
        }

        out match {
          case i if i.valueAtRoot == Some("add" :: Nil) => Right(ListAction.Add)
          case i if i.valueAtRoot == Some("continue" :: Nil) => Right(ListAction.Continue)
          case i if i.isDefinedAt("edit" :: Nil) => readInt("edit", ListAction.Edit)(i)
          case i if i.isDefinedAt("delete" :: Nil) => readInt("delete", ListAction.Edit)(i)
          case _ => Left(ErrorMsg("invalid").toTree)
        }
      }

      def encode(in: ListAction): Input = in match {
        case ListAction.Add => Input.one("add" :: Nil)
        case ListAction.Edit(i) => Input.one(i.toString :: Nil).prefixWith("edit")
        case ListAction.Delete(i) => Input.one(i.toString :: Nil).prefixWith("delete")
        case ListAction.Continue => Input.one("continue" :: Nil)
      }
      def render(
        pageKey: List[String],
        fieldKey: List[String],
        tell: Option[Html],
        breadcrumbs: Breadcrumbs,
        data: Input,
        errors: ErrorTree,
        messages: UniformMessages[Html]
      ): Option[Html] = {

        val indexedRows: List[ListingRow[Html]] = existingEntries.zipWithIndex.
          sorted(orderWithIndex).
          map { case (v, index) =>
            val editLink = s"$key/edit/$index/"
            val deleteLink = s"$key/delete/$index/"
            ListingRow[Html](
              index,
              tell,
              editLink,
              deleteLink
            )
          }

        Some(renderListPage(
          pageKey,
          breadcrumbs,
          indexedRows,
          data,
          errors,
          messages,
          validation
        ))
      }
    }
    new StandardTellAndAskForm(tell, ff) {
      override val customRouting: PartialFunction[List[String],ListAction] = {
        case "edit" :: Pos(x) :: _   => ListAction.Edit(x)
        case "delete" :: Pos(x) :: Nil => ListAction.Delete(x)
      }
    }
  }
  

  implicit def fromTell[A](
    implicit tell: GenericWebTell[Html, A],
    codec: Codec[A]
  ) = new WebAskList[Html, A] {
    def apply(
      key: String,
      askJourney: (Option[Int], List[A]) => WebMonad[Html,A],
      default: Option[List[A]],
      validation: Rule[List[A]],
      customContent: Map[String,(String, List[Any])]
    ) = new WebMonad[Html,List[A]] {

      implicit def listCodec = new Codec[List[A]] {

        def decode(out: Input): Either[ErrorTree,List[A]] = {

          @annotation.tailrec
          def inner(index: Int, acc: List[A]): Either[ErrorTree,List[A]] = {
            (out /? index.toString) match {
              case Some(e) =>
                codec.decode(e) match {
                  case Left(e) => Left(e.prefixWith(index.toString))
                  case Right(v) => inner(index + 1, v :: acc)
                }
              case None =>
                Right(acc)
            }
          }

          inner(0, Nil)
        }

        def encode(in: List[A]): Input = {
          in.zipWithIndex.map {
            case (e, i) => codec.encode(e).prefixWith(i.toString)
          }.combineAll
        }
      }

      def apply(pageIn: PageIn[Html])(implicit ec: ExecutionContext): Future[PageOut[Html,List[A]]] = {

        import pageIn._

        val data : List[A] = (
          state.get(key :: "zzdata" :: Nil) >>=
            (Input.fromUrlEncodedString(_).toOption) >>=
            (listCodec.decode(_).toOption)
        ).orElse(default).getOrElse(List.empty)

        println(state)

        val min = 1  // TODO

        val decision: WebMonad[Html, ListAction] = if (config.askFirstListItem && data.isEmpty && min > 0) {
            (ListAction.Add: ListAction).pure[WebMonad[Html, ?]]
        } else {
          // (id: String, tell: Option[Unit], defaultIn: Option[ltbs.uniform.common.web.ListAction], validationIn: ltbs.uniform.validation.Rule[ltbs.uniform.common.web.ListAction], customContent: Map[String,(String, List[Any])])
          val wi = listingForm(key, data, validation, tell)
          wi.apply(key, None, None, Rule.alwaysPass, Map.empty)
        }

        def deleteJourney(data: List[A], index: Int) = true.pure[WM]

        val wm = decision flatMap {
          case ListAction.Continue =>
            data.pure[WM]

          case ListAction.Add =>
            for {
              _ <- pushPathPrefix(key :: "add" :: Nil)
              r <- askJourney(None, data)
              _ <- db.update(List(key, "zzdata"), r :: data)(listCodec)
              _ <- db.deleteRecursive(key :: "add" :: Nil)
              _ <- popPathPrefix(2)
              _ <- goto[Unit](key)
            } yield (List.empty[A])

            case ListAction.Delete(index) => for {
              _ <- pushPathPrefix(key :: "delete" :: index.toString :: Nil)
              confirm <- deleteJourney(data, index)
              _ <- confirm match {
                case true =>
                  db.update(List(key, "zzdata"),data.deleteAtIndex(index))(listCodec)
                case false =>
                  ().pure[WM]
              }
              _ <- db.deleteRecursive(key :: "delete" :: index.toString :: Nil)
              _ <- popPathPrefix(3)
              _ <- goto[Unit](key)
            } yield (List.empty[A])

            case ListAction.Edit(index) => {
              for {
                _ <- pushPathPrefix(key :: "edit" :: index.toString :: Nil)
                r <- askJourney(Some(index), data)
                _ <- db.update(List(key, "zzdata"),data.replaceAtIndex(index, r))(listCodec)
                _ <- db.deleteRecursive(key :: "edit" :: index.toString :: Nil)
                _ <- popPathPrefix(3)
                _ <- goto[Unit](key)
              } yield (List.empty[A])
            }
        }

        wm(pageIn)
      }
    }

    def deleteConfirmationJourney: Uniform[Needs[_,_], Any, Boolean] =
      pure(true)
  }
  
}

