package ltbs.uniform.webmonad

import cats.Monoid
import cats.{ Invariant, Functor }
import cats.data._
import cats.implicits._
import org.atnos.eff._
import org.atnos.eff.all._
import org.atnos.eff.syntax.all._
import play.api.data.Form
import play.twirl.api.Html
import scala.concurrent.{ ExecutionContext, Future }
import ltbs.uniform._
import play.api._
import play.api.mvc._
import org.atnos.eff.future._
import org.atnos.eff.syntax.future._

trait PlayInterpreter extends Controller {

  val log: Logger = Logger("uniform")

  def formToValidated[A](f: Form[A]): ValidatedData[A] =
    if (!f.hasErrors) f.value.map{_.valid}
    else Some(f.errors.head.message.invalid)

  trait WebMonadForm[T] {
    def render(key: String, existing: ValidatedData[T], request: Request[AnyContent]): Html
    def encode(in: T): Encoded
    def decode(out: Encoded): T
    def playForm(key: String, validation: T => Validated[ValidationError, T]): Form[T]
  }

  trait WebMonadSelectPage[T] {
    def toHtml(in: T): Html
    def render(key: String, options: Set[T], existing: ValidatedData[T], request: Request[AnyContent]): Html
    def encode(in: T): Encoded
    def decode(out: Encoded): T
    def playForm(key: String, validation: T => Validated[ValidationError, T]): Form[T]
  }

  implicit val playFormFunctor: Invariant[Form] = new Invariant[Form]{
    def imap[A, B](fa: Form[A])(f: A => B)(g: B => A): Form[B] =
      new Form[B](fa.mapping.transform(f, g), fa.data, fa.errors, fa.value.map(f))
  }

  implicit val invariantWebMonad: Invariant[WebMonadForm] = new Invariant[WebMonadForm] {

    def imap[A, B](fa: WebMonadForm[A])(f: A => B)(g: B => A): WebMonadForm[B] = new WebMonadForm[B]{
      def render(key: String, existing: ValidatedData[B], request: Request[AnyContent]): Html =
        fa.render(key, existing.map{_.map(g)}, request)
      def encode(in: B): Encoded = fa.encode(g(in))
      def decode(out: Encoded): B = f(fa.decode(out))
      def playForm(key: String, validation: B => Validated[ValidationError, B]): Form[B] =
        fa.playForm(key, { a: A => validation(f(a)).map(g) }).imap(f)(g)
    }
  }

  implicit val invariantWebMonadSelect: Invariant[WebMonadSelectPage] = new Invariant[WebMonadSelectPage] {

    def imap[A, B](fa: WebMonadSelectPage[A])(f: A => B)(g: B => A): WebMonadSelectPage[B] = new WebMonadSelectPage[B]{
      def toHtml(in: B): Html = fa.toHtml(g(in))
      def render(key: String, options: Set[B], existing: ValidatedData[B], request: Request[AnyContent]): Html =
        fa.render(key, options.map(g), existing.map{_.map(g)}, request)
      def encode(in: B): Encoded = fa.encode(g(in))
      def decode(out: Encoded): B = f(fa.decode(out))
      def playForm(key: String, validation: B => Validated[ValidationError, B]): Form[B] =
        fa.playForm(key, { a: A => validation(f(a)).map(g) }).imap(f)(g)
    }
  }

  type PlayStack = Fx.fx6[Reader[String, ?], Reader[Request[AnyContent], ?], State[DB, ?], State[List[String],?], Either[Result, ?], TimedFuture]

  implicit class PlayEffectOps[R, A](e: Eff[R, A]) {
    type _readStage[Q] = Reader[String,?] |= Q
    type _readRequest[Q] = Reader[Request[AnyContent],?] |= Q
    type _db[Q]  = State[DB,?] |= Q
    type _breadcrumbs[Q]  = State[List[String],?] |= Q
    type _timedFuture[Q]  = TimedFuture[?] |= Q
    type _either[Q] = Either[Result,?] |= Q

    private def pageLogic[S: _readRequest: _readStage: _db: _breadcrumbs: _timedFuture: _either,A](
      id: String,
      render: (String, Option[Validated[ValidationError, A]], Request[AnyContent]) => Html,
      decode: Encoded => A,
      encode: A => Encoded,
      form: Form[A]
    ): Eff[S,A] ={

      object ValidDecode {
        def unapply(in: Option[Encoded]): Option[A] = in.map(decode)
      }

      for {
        request <- ask[S, Request[AnyContent]]
        targetId <- ask[S, String]
        method = request.method.toLowerCase
        state <- get[S, DB]
        data = state.get(id)
        breadcrumbs <- get[S, List[String]]
        ret <- (method, data, targetId) match {

          case ("get", None, `id`) =>
            log.info("nothing in database, step in URI, render empty form")
            left[S, Result, A](Ok(render(id, None, request)))

          case ("get", ValidDecode(data), `id`) =>
            log.info("something in database, step in URI, user revisiting old page, render filled in form")
            left[S, Result, A](Ok(render(id, Some(data.valid[ValidationError]), request)))

          // TODO: Check validation too
          case ("get", ValidDecode(data), _) =>
            log.info("something in database, not step in URI, pass through")
            put[S, List[String]](id :: breadcrumbs) >>
              Eff.pure[S, A](data.asInstanceOf[A])

          case ("post", _, `id`) =>
            form
              .bindFromRequest()(request).fold(

                formWithErrors => {
                  log.info("form submitted, step in URI, validation failure")
                  left[S, Result, A](BadRequest(render(id, formToValidated(formWithErrors), request)))
                },

                formData => {
                  log.info("form submitted, step in URI, validation pass")
                  put[S, List[String]](id :: breadcrumbs) >>
                    put[S, DB](state + (id -> encode(formData))) >>
                    Eff.pure[S, A](formData)
                }
              )

          case ("post", Some(data), _) if breadcrumbs.contains(targetId) =>
            log.info("something in database, previous page submitted")
            put[S, List[String]](id :: breadcrumbs) >>
              left[S, Result, A](Redirect(s"./$id"))

          // TODO: Check validation too
          case ("post", ValidDecode(data), _) =>
            log.info("something in database, posting, not step in URI nor previous page -> pass through")
            put[S, List[String]](id :: breadcrumbs) >>
              Eff.pure[S, A](data.asInstanceOf[A])

          case ("post", _, _) | ("get", _, _) =>
            log.info("nothing else seems applicable. maybe this should be a 404?")
            left[S, Result, A](Redirect(s"./$id"))
        }
      } yield ret
    }


    def useSelectPage[C,U](
      wmFormC: WebMonadSelectPage[C]
    )(
      implicit member: Member.Aux[UniformSelect[C,?], R, U],
      readStage: _readStage[U],
      readRequest: _readRequest[U],
      dbM: _db[U],
      breadcrumbsM: _breadcrumbs[U],
      futureM: _timedFuture[U],
      eitherM: _either[U]
    ): Eff[U, A] = e.translate(
      new Translate[UniformSelect[C,?], U] {
        def apply[X](ax: UniformSelect[C,X]): Eff[U, X] = {
          val wmForm: WebMonadSelectPage[X] = wmFormC.imap(_.asInstanceOf[X])(_.asInstanceOf[C])

          def form(id: String, validationError: Option[Validated[ValidationError, X]], request: Request[AnyContent]) =
            ax match {
              case a: UniformSelectOne[U,X] => wmForm.render(id, a.options, validationError, request)
              case a: UniformSelectMany[U,X] => wmForm.render(id, a.options, validationError, request)
            }

          val options: Set[X] = ax match {
            case a: UniformSelectOne[U,X] => a.options
            case a: UniformSelectMany[U,X] => a.options
          }

          (ax.key, ax.validation) match {
            case (id, validation) =>

              object ValidDecode {
                def unapply(in: Option[Encoded]): Option[X] = in.map(wmForm.decode)
              }

              val i: Eff[U,X] = pageLogic[U,X](
                id,
                wmForm.render(_,options, _, _),
                wmForm.decode,
                wmForm.encode,
                wmForm.playForm(id, validation(_))
              )
              i
          }
        }
      }
    )


    def useForm[C, U](
      wmFormC: WebMonadForm[C]
    )(
      implicit member: Member.Aux[UniformAsk[C,?], R, U],
      readStage: _readStage[U],
      readRequest: _readRequest[U],
      dbM: _db[U],
      breadcrumbsM: _breadcrumbs[U],
      futureM: _timedFuture[U],
      eitherM: _either[U]
    ): Eff[U, A] = e.translate(
      new Translate[UniformAsk[C,?], U] {
        def apply[X](ax: UniformAsk[C,X]): Eff[U, X] = {
          val wmForm: WebMonadForm[X] = wmFormC.imap(_.asInstanceOf[X])(_.asInstanceOf[C])

          def form(id: String, validationError: Option[Validated[ValidationError, X]], request: Request[AnyContent]) =
            ax match {
              case a: UniformAsk[U,X] => wmForm.render(id, validationError, request)
            }

          (ax.key, ax.validation) match {
            case (id, validation) =>

              object ValidDecode {
                def unapply(in: Option[Encoded]): Option[X] = in.map(wmForm.decode)
              }

              val i: Eff[U,X] = pageLogic[U,X](
                id,
                wmForm.render,
                wmForm.decode,
                wmForm.encode,
                wmForm.playForm(id, validation(_))
              )
              i
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
        case ((Left(result), db), breadcrumbs) =>
          persistence.dataPut(db).map(_ => result)
        case ((Right(a), db), breadcrumbs) =>
          val newDb: DB = if (purgeJourneyOnCompletion) (Monoid[DB].empty) else db
          persistence.dataPut(newDb) >> terminalFold(a)
      }
    }
}
