package controllers


import cats.implicits._
import cats.kernel.Monoid
import javax.inject._
import ltbs.uniform.datapipeline.{Messages => _, _}
import ltbs.uniform.interpreters.playframework._
import ltbs.uniform.sampleprograms.BeardTax._
import ltbs.uniform.widgets.govuk._
import ltbs.uniform.common.web._
import org.atnos.eff._
import play.api._
import play.api.i18n._
import play.api.mvc._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent._
import play.twirl.api.Html

import InferParser._

@Singleton
class SimplifiedBeardController @Inject()(implicit val messagesApi: MessagesApi) extends Controller with PlaySimplifiedInterpreter with I18nSupport {


  val persistence = new Persistence {
    private var data: DB = Monoid[DB].empty
    def dataGet: Future[DB] = Future.successful(data)
    def dataPut(dataIn: DB): Future[Unit] =
      Future(data = dataIn).map{_ => ()}
  }


  def beardAction(key: String) = {

    def `inferForm''`[A](
      implicit
        parser: DataParser[A],
      html: HtmlForm[A]
    ) = new SimpleInteractionForm[Input,A,Html] {

      import java.net.URLEncoder.{encode => urlencode}
      import java.net.URLDecoder.{decode => urldecode}

      def decode(out: Encoded): Either[ErrorTree,A] = parser.bind(out)
      def encode(in: A): Encoded = ???
      def receiveInput(data: Input): Encoded = {
        // TODO: No more bodged, non stack-safe tree implementations
        def inner(subKey: String, subInput: Input): List[String] = {
          subInput.value.map{ v => s"$subKey=$v" } ++
          subInput.children.flatMap{
            case (k,f) => inner(s"${subKey}.$k", f)
          }
        }
        inner("root", data)
      }.mkString("&")
      def render(key: String, existing: Option[Encoded], data: Input, errors: ErrorTree): Html = ???

      def decodeUrlString(input: String): FormUrlEncoded =
        input.split("&")
          .toList.map(_.trim).filter(_.nonEmpty)
          .groupBy(_.takeWhile(_ != '='))
          .map { case (k,v) =>
            val key = urldecode(k, "UTF-8").replace("[]", "")
            val value = v.map(x => urldecode(x.dropWhile(_ != '=').drop(1), "UTF-8"))
            (key, value)
          }

      def encodeUrlString(input: FormUrlEncoded): String =
        input.toList.flatMap{
          case (f,vs) => vs.map{v =>
            val encoded = urlencode(v, "UTF-8")
            s"$f=$encoded"
          }
        }.mkString("&")


    }


    def `inferForm'`[A](
      implicit
        parser: DataParser[A],
      html: HtmlForm[A]
    ) = new SimpleInteractionForm[FormUrlEncoded,A,Html] {

      import java.net.URLEncoder.{encode => urlencode}
      import java.net.URLDecoder.{decode => urldecode}

      def decodeUrlString(input: String): FormUrlEncoded =
        input.split("&")
          .toList.map(_.trim).filter(_.nonEmpty)
          .groupBy(_.takeWhile(_ != '='))
          .map { case (k,v) =>
            val key = urldecode(k, "UTF-8").replace("[]", "")
            val value = v.map(x => urldecode(x.dropWhile(_ != '=').drop(1), "UTF-8"))
            (key, value)
          }

      def decode(out: Encoded): Either[ErrorTree,A] = ???
      def encode(in: A): Encoded = ???
      def receiveInput(data: FormUrlEncoded): Encoded =
        data.toList.flatMap{
          case (f,vs) => vs.map{v =>
            val encoded = urlencode(v, "UTF-8")
            s"$f=$encoded"
          }
        }.mkString("&")
        
      def render(key: String,existing: Option[Encoded],data: FormUrlEncoded,errors: ErrorTree): Html = ???
    }


    def inferForm[A](
      implicit
        parser: DataParser[A],
      html: HtmlForm[A],
      request: Request[AnyContent]
    ) = new PlayForm[A] {
      def decode(out: Encoded): Either[ErrorTree,A] = ???
      def encode(in: A): Encoded = ???
      def receiveInput(data: play.api.mvc.Request[AnyContent]): Encoded = ???
      def render(key: String,existing: Option[Encoded],data: play.api.mvc.Request[AnyContent],errors: ErrorTree): play.twirl.api.Html = ???
    }

    Action.async { implicit request =>
      runWeb(
        program = program[FxAppend[TestProgramStack, PlayStack]]
          .useForm(inferForm[Option[MemberOfPublic]])
          .useForm(inferForm[BeardStyle])
          .useForm(inferForm[BeardLength]),
        key,
        request,
        persistence
      )(
        a => Future.successful(Ok(s"You have £$a to pay"))
      )
    }
  }

}
