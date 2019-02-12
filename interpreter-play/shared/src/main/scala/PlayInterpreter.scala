package ltbs.uniform.interpreters.playframework

import cats.Monoid
import cats.data._
import cats.implicits._
import org.atnos.eff._
import org.atnos.eff.all.{none => _, _}
import org.atnos.eff.syntax.all._
import play.api.data.Form
import ltbs.uniform._
import play.api._
import play.api.mvc._
import play.twirl.api.Html
import ltbs.uniform._
import ltbs.uniform.web._
import scala.concurrent.{ ExecutionContext, Future }
import ltbs.uniform.DB._

trait PlayInterpreter extends Compatibility.PlayController {

  def messages(request: Request[AnyContent]): Messages

  def renderForm(
    key: List[String],
    errors: ErrorTree,
    tell: Html,
    form: Html,
    breadcrumbs: List[String],
    request: Request[AnyContent],
    messages: Messages
  ): Html

  def listingPage[A : Htmlable](
    key: List[String],
    errors: ErrorTree,
    elements: List[A],
    messages: Messages
  ): Html

  implicit def convertMessages(implicit input: i18n.Messages): Messages = new Messages{
    override def apply(key: List[String],args: Any*): String = input(key, args)
    override def apply(key: String,args: Any*): String = input(key, args)
    def get(key: String,args: Any*): Option[String] = if (input.isDefinedAt(key))
      input.messages(key, args:_*).some
    else
      none[String]

    def get(key: List[String],args: Any*): Option[String] = key collectFirst {
      case k if input.isDefinedAt(k) => input.messages(k, args:_*)
    }

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

  type PlayStack = Fx.fx2[State[(DB,List[List[String]]), ?], Either[Result, ?]]

  type _state[Q]  = State[(DB, List[List[String]]),?] |= Q
  type _either[Q] = Either[Result,?] |= Q

  // for some reason the compiler isn't letting me inline this as a lambda within delist
  private def alwaysYes[NEWSTACK,OUT](a: List[String], b: List[OUT], c:OUT): Eff[NEWSTACK, Boolean] =
    Eff.pure[NEWSTACK,Boolean](true)

  implicit class PlayEffectOps[STACK, A](e: Eff[STACK, A]) {

    def useForm[OUT, NEWSTACK](
      wmFormC: PlayForm[OUT]
    )(
      implicit member: Member.Aux[UniformAsk[OUT,?], STACK, NEWSTACK],
      state: _state[NEWSTACK],
      eitherM: _either[NEWSTACK],
      request: Request[AnyContent],
      targetId: List[String]
    ): Eff[NEWSTACK, A] =
      useFormMap[Unit,OUT,NEWSTACK]({_ => Html("")}, _ => wmFormC)

    def useFormMap[OUT, NEWSTACK](
      wmFormOUT: List[String] => PlayForm[OUT]      
    )(
      implicit member: Member.Aux[UniformAsk[OUT,?], STACK, NEWSTACK],
      state: _state[NEWSTACK],
      eitherM: _either[NEWSTACK],
      request: Request[AnyContent],
      targetId: List[String]
    ): Eff[NEWSTACK, A] =
      useFormMap[Unit,OUT,NEWSTACK]({_ => Html("")}, wmFormOUT)

    def useForm[IN, OUT, NEWSTACK](
      renderTell: IN => Html,      
      wmFormC: PlayForm[OUT]
    )(
      implicit member: Member.Aux[Uniform[IN,OUT,?], STACK, NEWSTACK],
      state: _state[NEWSTACK],
      eitherM: _either[NEWSTACK],
      request: Request[AnyContent],
      targetId: List[String]
    ): Eff[NEWSTACK, A] = useFormMap(renderTell, _ => wmFormC)

    def useFormMap[IN, OUT, NEWSTACK](
      renderTell: IN => Html,
      wmFormOUT: List[String] => PlayForm[OUT]
    )(
      implicit member: Member.Aux[Uniform[IN,OUT,?], STACK, NEWSTACK],
      stateM: _state[NEWSTACK],
      eitherM: _either[NEWSTACK],
      request: Request[AnyContent],
      targetId: List[String]
    ): Eff[NEWSTACK, A] = e.translate(
      new Translate[Uniform[IN, OUT,?], NEWSTACK] {
        def apply[X](ax: Uniform[IN, OUT,X]): Eff[NEWSTACK, X] = {
          val wmForm: PlayForm[X] = wmFormOUT(ax.key).imap(_.asInstanceOf[X])(_.asInstanceOf[OUT])

          val baseUrl = request.target.path.replaceFirst(targetId.mkString("/") + "$", "")

          def breadcrumbsToUrl(in: List[List[String]]): List[String] =
            in.map { xs => baseUrl + xs.mkString("/") }

          ax match {
            case Uniform(id, tell, default, validation) =>

              for {
                g <- get[NEWSTACK, (DB, List[List[String]])]                
                method = request.method.toLowerCase
                (state, breadcrumbs) = g
                dbObject: Option[OUT] = {
                  val o = state.get(id).flatMap(wmFormOUT(id).decode(_).flatMap(validation(_).toEither) match {
                    case Left(e) =>
                      log.warn(s"$id - serialised data present, but failed validation - $e")
                      None
                    case Right(r) => Some(r)
                  })
                  o
                }

                ret <- (method, dbObject, targetId) match {
                  case ("get", None, `id`) =>
                    log.info(s"$id - nothing in database, step in URI, render empty form")
                    left[NEWSTACK, Result, X](Ok(renderForm(id, Tree.empty,
                      renderTell(tell), wmForm.render(id.last, None, request),
                      breadcrumbsToUrl(breadcrumbs), request, messages(request)
                    )))

                  case ("get", Some(o), `id`) =>
                    val encoded = wmForm.encode(o.asInstanceOf[X]) // FormUrlEncoded.readString(wmForm.encode(o)).prefix(id).writeString
                    log.info(s"""|$id - something in database, step in URI, user revisiting old page, render filled in form
                                 |\t\t data: $o
                                 |\t\t encoded: $encoded """.stripMargin)
                    left[NEWSTACK, Result, X](Ok(
                      renderForm(id, Tree.empty,
                      renderTell(tell), wmForm.render(id.last, Some(encoded), request),
                      breadcrumbsToUrl(breadcrumbs), request, messages(request)
                    )))

                  case ("get", Some(data), _) =>
                    log.info(s"$id - something in database, not step in URI, pass through")
                    put[NEWSTACK, (DB,List[List[String]])]((state, id :: breadcrumbs)) >>
                    Eff.pure[NEWSTACK, X](data.asInstanceOf[X])

                  case ("post", _, `id`) =>
                    val data: Encoded =
                      wmForm.receiveInput(request)
                    
                    wmForm.decode(data) match {
                      case Left(errors) =>
                        log.info(s"$id - form submitted, step in URI, validation failure")
                        log.info(s"  errors: $errors")
                        log.info(s"  data: $data")                                                
                        left[NEWSTACK, Result, X](BadRequest(renderForm(id, errors,
                          renderTell(tell),
                          wmForm.render(id.last, Some(data), request, errors),
                          breadcrumbsToUrl(breadcrumbs), request, messages(request)
                        )))
                      case Right(o) =>
                        log.info(s"$id - form submitted, step in URI, validation pass")
                        put[NEWSTACK, (DB,List[List[String]])]((state + (id -> wmForm.encode(o)), id :: breadcrumbs)) >>
                        Eff.pure[NEWSTACK, X](o)
                    }

                  case ("post", Some(_), _) if breadcrumbs.contains(targetId) =>
                    log.info(s"$id - something in database, previous page submitted")
                    put[NEWSTACK, (DB, List[List[String]])]((state, id :: breadcrumbs)) >>
                    left[NEWSTACK, Result, X](Redirect(s"${baseUrl}${id.mkString("/")}"))

                  case ("post", Some(data), _) =>
                    log.info(s"$id - something in database, posting, not step in URI nor previous page -> pass through")
                    put[NEWSTACK, (DB,List[List[String]])]((state, id :: breadcrumbs)) >>
                      Eff.pure[NEWSTACK,X](data.asInstanceOf[X])

                  case ("post", _, _) | ("get", _, _) =>
                    log.warn(
                      s"""|$id - nothing else seems applicable. maybe this should be a 404?
                          |\t\t method:$method
                          |\t\t dbObject:$dbObject
                          |\t\t targetId:$targetId""".stripMargin)
                    
                    left[NEWSTACK, Result, X](Redirect(s"${baseUrl}${id.mkString("/")}"))
                }
              } yield ret
          }
        }
      }
    )


def delist[OUT, NEWSTACK, INNER](
      subJourneyP: (List[String], List[OUT], Option[OUT]) => Eff[INNER, OUT]
    )(
      implicit member: Member.Aux[UniformAsk[List[OUT],?], STACK, NEWSTACK],
      stateM: _state[NEWSTACK],
      eitherM: _either[NEWSTACK],
      listingPage: _uniform[List[OUT], ListControl, NEWSTACK],
      parser: DataParser[List[OUT]],
      f: IntoPoly[INNER,NEWSTACK]
    ): Eff[NEWSTACK,A] = e.translate(
      new Translate[UniformAsk[List[OUT],?], NEWSTACK] {

        val removeConfirmation: (List[String], List[OUT], OUT) => Eff[NEWSTACK, Boolean] = {alwaysYes[NEWSTACK, OUT] _}

        def subJourney(id: List[String], all: List[OUT], editing: Option[OUT]): Eff[NEWSTACK, OUT] = {
          subJourneyP(id, all, editing).into[NEWSTACK]
        }

        def serialise(in: List[OUT]): String = FormUrlEncoded.fromInputTree(parser.unbind(in)).writeString
        def deserialise(in: String): List[OUT] = parser.bind(FormUrlEncoded.readString(in).toInputTree) match {
          case Left(t) if t.isEmpty => Nil
          case Left(e) => throw new IllegalStateException(e.toString)
          case Right(r) => r
        }

        def apply[X](ax: UniformAsk[List[OUT],X]): Eff[NEWSTACK, X] = {

          def real: Eff[NEWSTACK,List[OUT]] = ax match {
            case Uniform(id, tell, default, validation) =>

              def read: Eff[NEWSTACK, Option[List[OUT]]] =
                get[NEWSTACK, (DB, List[List[String]])].map {
                  _._1.get("__data" :: id).map(deserialise)
                }

              def write(in: List[OUT]): Eff[NEWSTACK, Unit] = for {
                existing    <- get[NEWSTACK, (DB, List[List[String]])]
                (dbSerialised, bread) = existing
                dbUpdated = dbSerialised + ({"__data" :: id} -> serialise(in))
                _           <- put[NEWSTACK, (DB, List[List[String]])]((dbUpdated, bread))
              } yield (())

              def process(elements: List[OUT]): Eff[NEWSTACK,List[OUT]] = {
                uniformP[List[OUT],ListControl,NEWSTACK](id, elements) >>=
                {_ match {
                  case ltbs.uniform.web.Continue => Eff.pure[NEWSTACK,List[OUT]](elements)

                  case AddAnother =>
                    subJourney(id :+ "add", elements, None) >>= {x =>
                      clear(id) >>
                      clearRecursive(id :+ "add") >>
                      write(elements :+ x) >>
                      process(elements :+ x)}

                  case Edit(ordinal) =>
                    subJourney(id :+ "edit", elements, elements.get(ordinal)) >>= {x =>
                      clear(id) >>
                      clearRecursive(id :+ "edit") >>
                      write(elements.replace(ordinal, x)) >>
                      process(elements.replace(ordinal, x))}

                  case Delete(ordinal) =>
                    removeConfirmation(id, elements, elements(ordinal)) >>= {
                      if (_) { 
                        clear(id) >>
                        write(elements.delete(ordinal)) >>
                        left[NEWSTACK, Result, List[OUT]](Redirect(".."))
                      } else
                        left[NEWSTACK, Result, List[OUT]](Redirect(".."))
                    }
                }}
              }

              read >>= { state => process(state.orElse(default).getOrElse(Nil)) }

          }
          real.map{_.asInstanceOf[X]}
        }
      }
    )

  }

  implicit val scheduler = ExecutorServices.schedulerFromGlobalExecutionContext

  def runWeb[A](
    program: Eff[PlayStack, A],
    persistence: Persistence,
    purgeJourneyOnCompletion: Boolean = true
  )(
    terminalFold: A => Future[Result]
  )(implicit ec: ExecutionContext): Future[Result] =
    persistence.dataGet.map{
      data => program
        .runEither
        .runState((data, List.empty[List[String]]))
        .run
    } >>= {
      _ match {
        case (Left(result), (db, _)) =>
          persistence.dataPut(db).map(_ => result)
        case (Right(a), (db, _)) =>
          val newDb: DB = if (purgeJourneyOnCompletion) (Monoid[DB].empty) else db
          persistence.dataPut(newDb) >> terminalFold(a)
      }
    }
}
