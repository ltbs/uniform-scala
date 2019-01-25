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

trait PlayInterpreter extends Compatibility.PlayController {

  def messages(request: Request[AnyContent]): Messages

  def renderForm(
    key: String,
    errors: ErrorTree,
    form: Html,
    breadcrumbs: List[String],
    request: Request[AnyContent],
    messages: Messages
  ): Html

  def listingPage[A : Htmlable](
    key: String,
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

  type PlayStack = Fx.fx2[State[(DB,List[String]), ?], Either[Result, ?]]

  implicit class PlayEffectOps[R, A](e: Eff[R, A]) {
    type _state[Q]  = State[(DB, List[String]),?] |= Q
    type _either[Q] = Either[Result,?] |= Q

    def useForm[C, U](
      wmFormC: PlayForm[C]
    )(
      implicit member: Member.Aux[UniformAsk[C,?], R, U],
      state: _state[U],
      eitherM: _either[U],
      request: Request[AnyContent],
      targetId: String
    ): Eff[U, A] = useFormMap(_ => wmFormC)

    def useFormListMap[C: Htmlable, U](
      wmForm: String => PlayForm[C]
    )(
      implicit member: Member.Aux[UniformAskList[C,?], R, U],
      stateM: _state[U],
      eitherM: _either[U],
      request: Request[AnyContent],
      targetId: String
    ): Eff[U, A] = e.translate(
      new Translate[UniformAskList[C,?], U] {
        
        def apply[X](ax: UniformAskList[C,X]): Eff[U, X] = {

          def real: Eff[U,List[C]] = {
            val UniformAskList(id,min,max,vElement,vList) = ax

            val listingId = id
            val addId = id + "-add"
            val deleteId = id + "-delete"
            val editId = id + "-edit"

            def massDecode(in: Encoded): Either[ErrorTree,List[C]] = {
              in.split("|||").toList.map{wmForm(id).decode}.sequence
            }

            for {
              g <- get[U, (DB, List[String])]
              method = request.method.toLowerCase
              (state, breadcrumbs) = g
              dbObject: List[C] = {
                val o = state.get(id).toList.flatMap(massDecode(_) match {
                  case Left(e) =>
                    log.warn(s"$id - serialised data present, but failed validation - $e")
                    Nil
                  case Right(r) => r
                })
                o
              }

//              def editId: Int = ???

              ret <- (method, dbObject, targetId) match {
                case ("get", elements, `listingId`) =>
                  log.info(s"$id - listing page")
                  left[U, Result, List[C]](Ok(renderForm(id, Tree.empty,
                    listingPage(id, Tree.empty, elements, messages(request)),
                    breadcrumbs, request, messages(request)
                  )))
                case ("get", elements, `addId`) =>
                  log.info(s"$id - add page")
                  left[U, Result, List[C]](Ok(renderForm(id, Tree.empty,
                    wmForm(id).render(id, None, request, Tree.empty),
                    breadcrumbs, request, messages(request)
                  )))
                case ("post", elements, `addId`) =>
                  log.info(s"$id - add page")
                  ???                  
                case ("get", elements, `editId`) =>
                  log.info(s"$id - edit page")
                  left[U, Result, List[C]](Ok(renderForm(id, Tree.empty,
                    wmForm(id).render(id, None, request, Tree.empty),
                    breadcrumbs, request, messages(request)
                  )))
                case ("post", elements, `editId`) =>
                  log.info(s"$id - edit page")
                  ???                                    
                case ("get", elements, `deleteId`) =>
                  log.info(s"$id - delete page")
                  ???                                    
                case _ => ???
              }
            } yield (ret)
          }

          // eff loses the type safety, but X = List[C]
          real.map{_.asInstanceOf[X]}
        }
      }
    )


    def useFormMap[C, U](
      wmFormC: String => PlayForm[C]
    )(
      implicit member: Member.Aux[UniformAsk[C,?], R, U],
      stateM: _state[U],
      eitherM: _either[U],
      request: Request[AnyContent],
      targetId: String
    ): Eff[U, A] = e.translate(
      new Translate[UniformAsk[C,?], U] {
        def apply[X](ax: UniformAsk[C,X]): Eff[U, X] = {
          val wmForm: PlayForm[X] = wmFormC(ax.key).imap(_.asInstanceOf[X])(_.asInstanceOf[C])

          ax match {
            case UniformAsk(id, validation) =>

              for {
                g <- get[U, (DB, List[String])]                
                method = request.method.toLowerCase
                (state, breadcrumbs) = g
                dbObject: Option[X] = {
                  val o = state.get(id).flatMap(wmForm.decode(_).flatMap(validation(_).toEither) match {
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
                    left[U, Result, X](Ok(renderForm(id, Tree.empty,
                      wmForm.render(id, None, request),
                      breadcrumbs, request, messages(request)
                    )))

                  case ("get", Some(o), `id`) =>
                    val encoded = wmForm.encode(o) // FormUrlEncoded.readString(wmForm.encode(o)).prefix(id).writeString
                    log.info(s"""|$id - something in database, step in URI, user revisiting old page, render filled in form
                                 |\t\t data: $o
                                 |\t\t encoded: $encoded """.stripMargin)
                    left[U, Result, X](Ok(

                      renderForm(id, Tree.empty,
                      wmForm.render(id, Some(encoded), request),
                      breadcrumbs, request, messages(request)
                    )))

                  case ("get", Some(data), _) =>
                    log.info(s"$id - something in database, not step in URI, pass through")
                    put[U, (DB,List[String])]((state, id :: breadcrumbs)) >>
                    Eff.pure[U, X](data.asInstanceOf[X])

                  case ("post", _, `id`) =>
                    val data: Encoded =
                      wmForm.receiveInput(request)
                    
                    wmForm.decode(data) match {
                      case Left(errors) =>
                        log.info(s"$id - form submitted, step in URI, validation failure")
                        log.info(s"  errors: $errors")
                        log.info(s"  data: $data")                                                
                        left[U, Result, X](BadRequest(renderForm(id, errors,
                          wmForm.render(id, Some(data), request, errors),
                          breadcrumbs, request, messages(request)
                        )))
                      case Right(o) =>
                        log.info(s"$id - form submitted, step in URI, validation pass")
                        put[U, (DB,List[String])]((state + (id -> wmForm.encode(o)), id :: breadcrumbs)) >>
                        Eff.pure[U, X](o)
                    }

                  case ("post", Some(_), _) if breadcrumbs.contains(targetId) =>
                    log.info(s"$id - something in database, previous page submitted")
                    put[U, (DB, List[String])]((state, id :: breadcrumbs)) >>
                    left[U, Result, X](Redirect(s"./$id"))

                  case ("post", Some(data), _) =>
                    log.info(s"$id - something in database, posting, not step in URI nor previous page -> pass through")
                    put[U, (DB,List[String])]((state, id :: breadcrumbs)) >>
                      Eff.pure[U,X](data.asInstanceOf[X])

                  case ("post", _, _) | ("get", _, _) =>
                    log.warn(
                      s"""|$id - nothing else seems applicable. maybe this should be a 404?
                          |\t\t method:$method
                          |\t\t dbObject:$dbObject
                          |\t\t targetId:$targetId""".stripMargin)
                    
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
    persistence: Persistence,
    purgeJourneyOnCompletion: Boolean = true
  )(
    terminalFold: A => Future[Result]
  )(implicit ec: ExecutionContext): Future[Result] =
    persistence.dataGet.map{
      data => program
        .runEither
        .runState((data, List.empty[String]))
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
