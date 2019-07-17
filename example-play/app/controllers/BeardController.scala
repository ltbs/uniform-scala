package controllers

import cats.implicits._, cats.Monoid
import javax.inject._
import ltbs.uniform._, interpreters.playframework._, examples.beardtax._
import play.api.i18n.{Messages ⇒ _, _}
import play.api.mvc._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent._
import play.twirl.api.{Html, HtmlFormat}
import java.time.LocalDate
import cats.data.Validated

@Singleton
class BeardController @Inject()(
  implicit val messagesApi: MessagesApi
) extends PlayInterpreter[Html] with I18nSupport {

  def messages(
    request: Request[AnyContent],
    customContent: Map[String,(String, List[Any])]
  ): UniformMessages[Html] = (
    this.convertMessages(messagesApi.preferred(request)) |+|
      UniformMessages.bestGuess.map(HtmlFormat.escape)
  )

  def pageChrome(
    key: List[String],
    errors: ErrorTree,
    tell: Html,
    ask: Html,
    breadcrumbs: Path,
    request: Request[AnyContent],
    messages: UniformMessages[Html]
  ): Html =
      views.html.chrome(key, errors, Html(tell.toString + ask.toString), breadcrumbs)(messages, request)

  def selectionOfFields(
    inner: List[(String, (List[String], Path, Option[Input], ErrorTree, UniformMessages[Html]) ⇒ Html)]
  )(key: List[String], path: Path, values: Option[Input], errors: ErrorTree, messages: UniformMessages[Html]): Html = {
    val value: Option[String] = values.fold(none[String])(_.valueAtRoot.flatMap{_.headOption})
    views.html.uniform.radios(
      key,
      inner.map{_._1},
      value,
      errors,
      messages,
      inner.map{
        case(subkey,f) ⇒ subkey → f(key :+ subkey, path, values.map{_ / subkey}, errors / subkey, messages)
      }.filter(_._2.toString.trim.nonEmpty).toMap
    )
  }

  implicit val persistence: PersistenceEngine[Request[AnyContent]] = DebugPersistence(UnsafePersistence())

  implicit val twirlBigStringField = new FormField[BigString,Html] {
    import shapeless.tag
    def decode(out: Input): Either[ErrorTree,BigString] = {
      val root: Option[BigString] = {
        val asString = out.valueAtRoot
          .flatMap(_.filter(_.trim.nonEmpty).headOption)

        asString.map{tag[BigStringTag][String]}
      }

      root match {
        case None ⇒ Left(ErrorMsg("required").toTree)
        case Some(data) ⇒ Right(data)
      }
    }

    def encode(in: BigString): Input = Input.one(List(in))
    def render(
      key: List[String],
      path: Path,
      data: Option[Input],
      errors: ErrorTree,
      messages: UniformMessages[Html]
    ): Html = {
      val existingValue: String = data.flatMap(_.valueAtRoot.flatMap{_.headOption}).getOrElse("")
      views.html.uniform.textarea(key, existingValue, errors, messages)
    }
  }

  implicit val twirlBooleanField = new FormField[Boolean,Html] {
    def decode(out: Input): Either[ErrorTree,Boolean] = {
      val root: Option[String] = out.valueAtRoot
        .flatMap(_.filter(_.trim.nonEmpty).headOption)

      root match {
        case None ⇒ Left(ErrorMsg("required").toTree)
        case Some("TRUE") ⇒ Right(true)
        case Some("FALSE") ⇒ Right(false)
        case _ ⇒ Left(ErrorMsg("bad.value").toTree)
      }
    }

    def encode(in: Boolean): Input = in match {
      case true ⇒ Input.one(List("TRUE"))
      case false ⇒ Input.one(List("FALSE"))
    }

    def render(
      key: List[String],
      path: Path,
      data: Option[Input],
      errors: ErrorTree,
      messages: UniformMessages[Html]
    ): Html = {
      val existingValue: Option[String] = data.flatMap(_.valueAtRoot.flatMap{_.headOption})
      views.html.uniform.radios(key, List("TRUE","FALSE"), existingValue, errors, messages)
    }
  }


  implicit val twirlStringField = new FormField[String,Html] {
    def decode(out: Input): Either[ErrorTree,String] =
      out.valueAtRoot.flatMap(_.headOption).getOrElse("").asRight

    def encode(in: String): Input = Input.one(List(in))
    def render(
      key: List[String],
      path: Path,
      data: Option[Input],
      errors: ErrorTree,
      messages: UniformMessages[Html]
    ): Html = {
      val existingValue: String = data.flatMap(_.valueAtRoot.flatMap{_.headOption}).getOrElse("")
      views.html.uniform.string(key, existingValue, errors, messages)
    }
  }

  implicit val twirlNonEmptyStringField: FormField[NonEmptyString, Html] =
    twirlStringField.simap(x ⇒
      NonEmptyString.fromString(x) match {
        case Some(x) ⇒ Right(x)
        case None    ⇒ Left(ErrorMsg("required").toTree)
      }
    )(identity)

  implicit val twirlIntField2: FormField[Int,Html] =
    twirlStringField.simap(x ⇒
      Either.catchOnly[NumberFormatException](x.toInt)
        .leftMap(_ ⇒ ErrorMsg("bad.value").toTree)
    )(_.toString)


  implicit val twirlDateField = new FormField[LocalDate,Html] {

    def decode(out: Input): Either[ErrorTree,LocalDate] = {

      def intAtKey(key: String): Validated[ErrorTree, Int] =
        Validated.fromOption(
          out.valueAt(key).flatMap{_.filter(_.trim.nonEmpty).headOption},
          ErrorTree.oneErr(ErrorMsg("required")).prefixWith(key)
        ).andThen{
            x ⇒ Validated.catchOnly[NumberFormatException](x.toInt).leftMap(_ ⇒ ErrorMsg("badValue").toTree)
        }

      (
        intAtKey("year"),
        intAtKey("month"),
        intAtKey("day")
      ).tupled.toEither.flatMap{ case (y,m,d) ⇒
        Either.catchOnly[java.time.DateTimeException]{
          LocalDate.of(y,m,d)
        }.leftMap(_ ⇒ ErrorTree.oneErr(ErrorMsg("badDate")))
      }
    }

    def encode(in: LocalDate): Input = Map(
        List("year") → in.getYear(),
        List("month") → in.getMonthValue(),
        List("day") → in.getDayOfMonth()
      ).mapValues(_.toString.pure[List])

    def render(
      key: List[String],
      path: Path,
      data: Option[Input],
      errors: ErrorTree,
      messages: UniformMessages[Html]
    ): Html = {
      val existingValue: String = data.flatMap(_.valueAtRoot.flatMap{_.headOption}).getOrElse("")
      views.html.uniform.date(
        key,
        data.getOrElse(Input.empty),
        errors,
        messages
      )
    }
  }

  class HodConnector extends Hod[Future] {
    def costOfBeard(beardStyle: BeardStyle, length: BeardLength): Future[Int] =
      Future{
        Thread.sleep(2000)
        IdDummyHod.costOfBeard(beardStyle, length)
      }
  }

  def adaptedHod = new Hod[WebMonad] {
    val inner = new HodConnector
    def costOfBeard(beardStyle: BeardStyle, length: BeardLength): WebMonad[Int] =
      FutureAdapter.alwaysRerun.apply(inner.costOfBeard(beardStyle, length))
  }

  def beardAction(targetId: String) = Action.async { implicit request: Request[AnyContent] ⇒
    val playProgram = beardProgram(
      new FuturePlayInterpreter[TellTypes, AskTypes],
      adaptedHod
    )
    run(playProgram, targetId){ _ ⇒ Future.successful(Ok("Fin"))}
  }

}
