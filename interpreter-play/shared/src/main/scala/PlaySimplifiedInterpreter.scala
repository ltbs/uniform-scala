package ltbs.uniform.interpreters.playframework

import cats.Monoid
import cats.data._
import cats.implicits._
import org.atnos.eff._
import org.atnos.eff.all.{none => _, _}
import org.atnos.eff.syntax.all._
import play.api.data.Form
import scala.concurrent.{ ExecutionContext, Future }
import ltbs.uniform._
import play.api._
import play.api.mvc._
import play.twirl.api.Html
import org.atnos.eff.syntax.future._
import ltbs.uniform.datapipeline._
import ltbs.uniform.common.web._

trait PlaySimplifiedInterpreter extends Compatibility.PlayController {

  def renderForm(form: Html, breadcrumbs: List[String]): Html 

  implicit def convertMessages(implicit input: i18n.Messages): Messages = new Messages{
    def apply(key: List[String],args: Any*): String = input(key, args)
    def apply(key: String,args: Any*): String = input(key, args)
    def get(key: String,args: Any*): Option[String] = if (input.isDefinedAt(key))
      input.messages(key, args:_*).some
    else
      none[String]

    def list(key: String,args: Any*): List[String] = {
      @annotation.tailrec
      def inner(cnt: Int = 2, acc: List[String] = Nil): List[String] =
        get(s"$key.$cnt", args:_*) match {
          case Some(m) => inner(cnt+1, m :: acc)
          case None    => acc
        }

      List(key, s"$key.1").map(get(_, args)).flatten ++ inner().reverse
    }
  }

  val log: Logger = Logger("uniform")

  def formToValidated[A](f: Form[A]): ValidatedData[A] =
    if (!f.hasErrors) f.value.map{_.valid}
    else Some(f.errors.head.message.invalid)

  type PlayStack = Fx.fx6[Reader[String, ?], Reader[Request[AnyContent], ?], State[DB, ?], State[List[String],?], Either[Result, ?], TimedFuture]

  implicit class PlayEffectOps[R, A](e: Eff[R, A]) {
    type _readStage[Q] = Reader[String,?] |= Q
    type _readRequest[Q] = Reader[Request[AnyContent],?] |= Q
    type _db[Q]  = State[DB,?] |= Q
    type _breadcrumbs[Q]  = State[List[String],?] |= Q
    type _timedFuture[Q]  = TimedFuture[?] |= Q
    type _either[Q] = Either[Result,?] |= Q

    def useForm[C, U](
      wmFormC: PlayForm[C]
    )(
      implicit member: Member.Aux[UniformAsk[C,?], R, U],
      readStage: _readStage[U],
      readRequest: _readRequest[U],
      dbM: _db[U],
      breadcrumbsM: _breadcrumbs[U],
      eitherM: _either[U]
    ): Eff[U, A] = e.translate(
      new Translate[UniformAsk[C,?], U] {
        def apply[X](ax: UniformAsk[C,X]): Eff[U, X] = {
          val wmForm: PlayForm[X] = wmFormC.imap(_.asInstanceOf[X])(_.asInstanceOf[C])

          (ax.key, ax.validation) match {
            case (id, validation) =>

              for {
                request <- ask[U, Request[AnyContent]]
                targetId <- ask[U, String]
                method = request.method.toLowerCase
                state <- get[U, DB]
                dbrecord = state.get(id).map(wmForm.decode(_).flatMap(validation(_).toEither))
                breadcrumbs <- get[U, List[String]]
                ret <- (method, dbrecord, targetId) match {
                  case ("get", None, `id`) =>
                    log.info("nothing in database, step in URI, render empty form")
                    left[U, Result, X](Ok(renderForm(
                      wmForm.render(id, None, request),
                      breadcrumbs
                    )))

                  case ("get", Some(Right(o)), `id`) =>
                    log.info("something in database, step in URI, user revisiting old page, render filled in form")
                    left[U, Result, X](Ok(renderForm(
                      wmForm.render(id, Some(wmForm.encode(o)), request),
                      breadcrumbs
                    )))

                  case ("get", Some(Right(data)), _) =>
                    log.info("something in database, not step in URI, pass through")
                    put[U, List[String]](id :: breadcrumbs) >>
                    Eff.pure[U, X](data.asInstanceOf[X])

                  case ("post", _, `id`) =>
                    val data: Encoded = wmForm.receiveInput(request)
                    
                    wmForm.decode(data) match {
                      case Left(errors) =>
                        log.info("form submitted, step in URI, validation failure")
                        left[U, Result, X](BadRequest(renderForm(
                          wmForm.render(id, Some(data), request, errors),
                          breadcrumbs
                        )))
                      case Right(o) =>
                        log.info("form submitted, step in URI, validation pass")
                        put[U, List[String]](id :: breadcrumbs) >>
                        put[U, DB](state + (id -> wmForm.encode(o))) >>
                        Eff.pure[U, X](o)
                    }

                  case ("post", Some(_), _) if breadcrumbs.contains(targetId) =>
                    log.info("something in database, previous page submitted")
                    put[U, List[String]](id :: breadcrumbs) >>
                    left[U, Result, X](Redirect(s"./$id"))

                  case ("post", _, _) | ("get", _, _) =>
                    log.info("nothing else seems applicable. maybe this should be a 404?")
                    left[U, Result, X](Redirect(s"./$id"))
                }
              } yield ret
          }
        }
      }
    )
  }

  implicit val scheduler = ExecutorServices.schedulerFromGlobalExecutionContext

  def runWeb[A](
    program: Eff[PlayStack, A],
    key: String,
    request: Request[AnyContent],
    persistence: Persistence,
    purgeJourneyOnCompletion: Boolean = true
  )(
    terminalFold: A => Future[Result]
  )(implicit ec: ExecutionContext): Future[Result] =
    persistence.dataGet >>= {
      data => program.runReader(key)
        .runReader(request)
        .runEither
        .runState(data)
        .runState(List.empty[String])
        .runSequential
    } >>= {
      _ match {
        case ((Left(result), db), _) =>
          persistence.dataPut(db).map(_ => result)
        case ((Right(a), db), _) =>
          val newDb: DB = if (purgeJourneyOnCompletion) (Monoid[DB].empty) else db
          persistence.dataPut(newDb) >> terminalFold(a)
      }
    }
}
