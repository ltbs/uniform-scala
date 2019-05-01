package controllers


import cats.implicits._
import cats.kernel.Monoid
import javax.inject._
import ltbs.uniform._
import ltbs.uniform.web._
import ltbs.uniform.web.parser._
import ltbs.uniform.interpreters.playframework._
import ltbs.uniform.sampleprograms.BeardTaxUF4._
import ltbs.uniform.widgets.govuk._
import play.api._
import play.api.i18n.{Messages => _, _}
import play.api.mvc._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent._
import play.twirl.api.{Html, HtmlFormat}

@Singleton
class BeardControllerUF4 @Inject()(implicit val messagesApi: MessagesApi) extends Controller with PlayInterpreterUF4 with I18nSupport {

  def messages(request: Request[AnyContent]): UniformMessages[Html] = (
    convertMessages(messagesApi.preferred(request)) |+|
      UniformMessages.bestGuess.map(HtmlFormat.escape)
  )

  def renderForm(
    key: List[String],
    errors: ErrorTree,
    form: Html,
    breadcrumbs: List[String],
    request: Request[AnyContent],
    messagesIn: UniformMessages[Html]
  ): Html = {
    views.html.chrome(key.last, errors, form, breadcrumbs)(messagesIn, request)
  }

  val persistence = new Persistence {
    private var data: DB = Monoid[DB].empty
    def dataGet: Future[DB] = Future.successful(data)
    def dataPut(dataIn: DB): Future[Unit] =
      Future(data = dataIn).map{_ => ()}
  }

  var data : DB = Monoid[DB].empty

  trait Opt[A] {
    val optVal: Option[A]
  }

  implicit def autoOpt[A](implicit in: A = null): Opt[A] = new Opt[A]{
    val optVal = Option(in)
  }

  def beardAction(key: String) = {
    implicit val keys: List[String] = key.split("/").toList
    Action.async { implicit request =>

      implicit def fu2[A]: PlayTell[A] = new PlayTell[A] {
        def render(in: A): Html = Html(in.toString)
      }

      implicit def fu3[A]: PlayAsk[A] = new PlayAsk[A] {
        def promptUser: AskInput[A] => WebMonad[A] = { in => 
          val AskInput(id, tellHtml, default, validation, customContent) = in
          import cats.data._
          EitherT[WebInner, Result, A] {
            RWST { case ((config, targetId, request), (pathB, st)) =>
              val requestMethod = request.method.toLowerCase;
              val data = st.get(id)
              //              Future[(List[String], (List[String], scala.collection.immutable.Map[List[String],String]), Either[play.api.mvc.Result,A])]
              {
                (requestMethod, data, targetId) match {
                  case ("get", _, "") =>
                    log.debug(s"$id :: empty URI, redirecting to ./$id")
                    (
                      id.pure[List],
                      (path, st),
                      Redirect(s"./$id").asLeft[A]
                    )
                  case ("get", None, `id`) =>
                    log.debug(s"$id :: nothing in database, step in URI, render empty form")
                    (
                      id.pure[List],
                      (path, st),
                      Ok(render(path, formWithDefault, implicitly)).asLeft[A]
                    )
                  case ("get", Some(json), `id`) =>
                    log.debug(s"$id :: something in database, step in URI, user revisting old page, render filled in form")
                    (
                      id.pure[List],
                      (path, st),
                      Ok(render(path, form.fill(json.as[A]), implicitly)).asLeft[A]
                    )
                  case ("get", Some(json), _) =>
                    log.debug(s"$id :: something in database, not step in URI, pass through")
                    (
                      id.pure[List],
                      (id :: path, st),
                      json.as[A].asRight[Result]
                    )
                  case ("post", _, `id`) => form.bindFromRequest.fold(
                    formWithErrors => {
                      log.debug(s"$id :: errors in form, badrequest")
                      (
                        id.pure[List],
                        (path, st),
                        BadRequest(render(path, formWithErrors, implicitly)).asLeft[A]
                      )
                    },
                    formData => {
                      log.debug(s"$id :: form data passed, all good, update DB and pass through")
                      (
                        id.pure[List],
                        (id :: path, st + (id -> Json.toJson(formData))),
                        formData.asRight[Result]
                      )
                    }
                  )
                  case ("post", Some(json), _) if path.contains(targetId) && (config.mode == SingleStep || id.startsWith("edit-")) =>
                    log.debug(s"$id :: something in database, previous page submitted, single step => redirect to ./$id")
                    (
                      id.pure[List],
                      (id :: path, st),
                      Redirect(s"./$id").asLeft[A]
                    )
                  case ("post", Some(json), _) =>
                    log.debug(s"$id :: something in database, posting, not step in URI nor previous page -> pass through")
                    (
                      id.pure[List],
                      (id :: path, st),
                      json.as[A].asRight[Result]
                    )
                  case ("post" | "get", None, _) if config.mode == LeapAhead && default.isDefined && !id.startsWith("edit-") =>
                    log.debug(s"$id :: nothing in db but leap ahead is set and default is defined -> pass through")
                    (
                      id.pure[List],
                      (id :: path, st + (id -> Json.toJson(default.get))),
                      default.get.asRight[Result]
                    )
                  case ("post", _, _) | ("get", _, _) =>
                    log.debug(s"$id :: some other scenario, redirecting to ./$id")
                    (
                      id.pure[List],
                      (path, st),
                      Redirect(s"./$id").asLeft[A]
                    )
                }
              }.pure[Future]

            }
          }
        }
      }

      val config = ""
      program(new FuturePlayInterpreter).value
        .run((config, key, request), (List.empty[String], data))
        .flatMap { case (_, (path, state), a) => {
            ().pure[Future]
        }.map { _ =>
          a.fold(identity, _ => Ok("fin"))
        }}
      
    }
  }

}
