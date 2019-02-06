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
    tell: Html,
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

  type _state[Q]  = State[(DB, List[String]),?] |= Q
  type _either[Q] = Either[Result,?] |= Q

  // for some reason the compiler isn't letting me inline this as a lambda within delist
  private def alwaysYes[NEWSTACK,OUT](a: String, b: List[OUT], c:OUT): Eff[NEWSTACK, Boolean] =
    Eff.pure[NEWSTACK,Boolean](true)

  implicit class PlayEffectOps[STACK, A](e: Eff[STACK, A]) {
    type _state[Q]  = State[(DB, List[String]),?] |= Q
    type _either[Q] = Either[Result,?] |= Q

    def delist[OUT, NEWSTACK, INNER](
      subJourneyP: (String, List[OUT], Option[OUT]) => Eff[INNER, OUT]
    )(
      implicit member: Member.Aux[UniformAsk[List[OUT],?], STACK, NEWSTACK],
      stateM: _state[NEWSTACK],
      eitherM: _either[NEWSTACK],
      listingPage: _uniform[List[OUT], ListControl, NEWSTACK],
      parser: DataParser[List[OUT]],
      f: IntoPoly[INNER,NEWSTACK]
    ): Eff[NEWSTACK,A] = e.translate(
      new Translate[UniformAsk[List[OUT],?], NEWSTACK] {

        val removeConfirmation: (String, List[OUT], OUT) => Eff[NEWSTACK, Boolean] = {alwaysYes[NEWSTACK, OUT] _}

        def subJourney(id: String, all: List[OUT], editing: Option[OUT]): Eff[NEWSTACK, OUT] = {
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
                get[NEWSTACK, (DB, List[String])].map { _._1.get(id + "--data").map(deserialise) }

              def write(in: List[OUT]): Eff[NEWSTACK, Unit] = for {
                existing    <- get[NEWSTACK, (DB, List[String])]
                (dbSerialised, bread) = existing
                dbUpdated = dbSerialised + ({id + "--data"} -> serialise(in))
                _ = {println(s"Setting $id--data to $in")}
                _           <- put[NEWSTACK, (DB, List[String])]((dbUpdated, bread))
              } yield (())

              def clear(key: String): Eff[NEWSTACK, Unit] = for {
                existing    <- get[NEWSTACK, (DB, List[String])]
                (dbSerialised, bread) = existing
                dbUpdated = dbSerialised - key
                _ = {println(s"Clearing $key - " ++ dbSerialised.keys.toString ++ " => " ++ dbUpdated.keys.toString)}
                _           <- put[NEWSTACK, (DB, List[String])]((dbUpdated, bread))
              } yield (())

              def process(elements: List[OUT]): Eff[NEWSTACK,List[OUT]] = {
                //              write(elements) >>
                uniform[List[OUT],ListControl,NEWSTACK](id, elements) >>=
                {_ match {
                  case ltbs.uniform.web.Continue => Eff.pure[NEWSTACK,List[OUT]](elements)

                  case AddAnother =>
                    subJourney(id, elements, None) >>= {x => clear(id) >> clear(s"$id-add") >> write(elements :+ x) >> process(elements :+ x)}
                    /*subjourney (id,elements,None)*/

                  case Edit(ordinal) =>
                    subJourney(id, elements, elements.get(ordinal)) >>= {x => clear(id) >> clear(s"$id-edit-$ordinal") >> write(elements.replace(ordinal, x)) >> process(elements.replace(ordinal, x))}
                    /*subjourney (id,elements,elements.get(ordinal)) >>= */

                  case Delete(ordinal) =>
                    removeConfirmation(id, elements, elements(ordinal)) >>= {
                      if (_)
                        clear(id) >> write(elements.delete(ordinal)) >> left[NEWSTACK, Result, List[OUT]](
                          Redirect(id)
                        )
                      else
                        left[NEWSTACK, Result, List[OUT]](
                          Redirect(id)
                        )
                    }
                }}
              }

              read >>= { state => process(state.orElse(default).getOrElse(Nil)) }

          }
          real.map{_.asInstanceOf[X]}
        }
      }
    )

    def useFormListMap[C: Htmlable, NEWSTACK](
      wmForm: String => PlayForm[C]
    )(
      implicit member: Member.Aux[UniformAskList[C,?], STACK, NEWSTACK],
      stateM: _state[NEWSTACK],
      eitherM: _either[NEWSTACK],
      request: Request[AnyContent],
      targetId: String
    ): Eff[NEWSTACK, A] = e.translate(
      new Translate[UniformAskList[C,?], NEWSTACK] {
        
        def apply[X](ax: UniformAskList[C,X]): Eff[NEWSTACK, X] = {

          def real: Eff[NEWSTACK,List[C]] = {
            val UniformAskList(id,min,max,vElement,vList) = ax

            val listingId = id
            val addId = id + "-add"
            val deleteId = id + "-delete"
            val editId = id + "-edit"

            def massDecode(in: Encoded): Either[ErrorTree,List[C]] = {
              in.split("|||").toList.map{wmForm(id).decode}.sequence
            }

            for {
              g <- get[NEWSTACK, (DB, List[String])]
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
                  left[NEWSTACK, Result, List[C]](Ok(renderForm(id, Tree.empty,
                    Html(""), listingPage(id, Tree.empty, elements, messages(request)),
                    breadcrumbs, request, messages(request)
                  )))
                case ("get", elements, `addId`) =>
                  log.info(s"$id - add page")
                  left[NEWSTACK, Result, List[C]](Ok(renderForm(id, Tree.empty,
                    Html(""), wmForm(id).render(id, None, request, Tree.empty),
                    breadcrumbs, request, messages(request)
                  )))
                case ("post", elements, `addId`) =>
                  log.info(s"$id - add page")
                  ???                  
                case ("get", elements, `editId`) =>
                  log.info(s"$id - edit page")
                  left[NEWSTACK, Result, List[C]](Ok(renderForm(id, Tree.empty,
                    Html(""), wmForm(id).render(id, None, request, Tree.empty),
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

    def useForm[OUT, NEWSTACK](
      wmFormC: PlayForm[OUT]
    )(
      implicit member: Member.Aux[UniformAsk[OUT,?], STACK, NEWSTACK],
      state: _state[NEWSTACK],
      eitherM: _either[NEWSTACK],
      request: Request[AnyContent],
      targetId: String
    ): Eff[NEWSTACK, A] =
      useFormMap[Unit,OUT,NEWSTACK]({_ => Html("")}, _ => wmFormC)

    def useFormMap[OUT, NEWSTACK](
      wmFormOUT: String => PlayForm[OUT]      
    )(
      implicit member: Member.Aux[UniformAsk[OUT,?], STACK, NEWSTACK],
      state: _state[NEWSTACK],
      eitherM: _either[NEWSTACK],
      request: Request[AnyContent],
      targetId: String
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
      targetId: String
    ): Eff[NEWSTACK, A] = useFormMap(renderTell, _ => wmFormC)

    def useFormMap[IN, OUT, NEWSTACK](
      renderTell: IN => Html,
      wmFormOUT: String => PlayForm[OUT]
    )(
      implicit member: Member.Aux[Uniform[IN,OUT,?], STACK, NEWSTACK],
      stateM: _state[NEWSTACK],
      eitherM: _either[NEWSTACK],
      request: Request[AnyContent],
      targetId: String
    ): Eff[NEWSTACK, A] = e.translate(
      new Translate[Uniform[IN, OUT,?], NEWSTACK] {
        def apply[X](ax: Uniform[IN, OUT,X]): Eff[NEWSTACK, X] = {
          val wmForm: PlayForm[X] = wmFormOUT(ax.key).imap(_.asInstanceOf[X])(_.asInstanceOf[OUT])

          ax match {
            case Uniform(id, tell, default, validation) =>

              for {
                g <- get[NEWSTACK, (DB, List[String])]                
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
                      renderTell(tell), wmForm.render(id, None, request),
                      breadcrumbs, request, messages(request)
                    )))

                  case ("get", Some(o), `id`) =>
                    val encoded = wmForm.encode(o.asInstanceOf[X]) // FormUrlEncoded.readString(wmForm.encode(o)).prefix(id).writeString
                    log.info(s"""|$id - something in database, step in URI, user revisiting old page, render filled in form
                                 |\t\t data: $o
                                 |\t\t encoded: $encoded """.stripMargin)
                    left[NEWSTACK, Result, X](Ok(
                      renderForm(id, Tree.empty,
                      renderTell(tell), wmForm.render(id, Some(encoded), request),
                      breadcrumbs, request, messages(request)
                    )))

                  case ("get", Some(data), _) =>
                    log.info(s"$id - something in database, not step in URI, pass through")
                    put[NEWSTACK, (DB,List[String])]((state, id :: breadcrumbs)) >>
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
                          wmForm.render(id, Some(data), request, errors),
                          breadcrumbs, request, messages(request)
                        )))
                      case Right(o) =>
                        log.info(s"$id - form submitted, step in URI, validation pass")
                        put[NEWSTACK, (DB,List[String])]((state + (id -> wmForm.encode(o)), id :: breadcrumbs)) >>
                        Eff.pure[NEWSTACK, X](o)
                    }

                  case ("post", Some(_), _) if breadcrumbs.contains(targetId) =>
                    log.info(s"$id - something in database, previous page submitted")
                    put[NEWSTACK, (DB, List[String])]((state, id :: breadcrumbs)) >>
                    left[NEWSTACK, Result, X](Redirect(s"./$id"))

                  case ("post", Some(data), _) =>
                    log.info(s"$id - something in database, posting, not step in URI nor previous page -> pass through")
                    put[NEWSTACK, (DB,List[String])]((state, id :: breadcrumbs)) >>
                      Eff.pure[NEWSTACK,X](data.asInstanceOf[X])

                  case ("post", _, _) | ("get", _, _) =>
                    log.warn(
                      s"""|$id - nothing else seems applicable. maybe this should be a 404?
                          |\t\t method:$method
                          |\t\t dbObject:$dbObject
                          |\t\t targetId:$targetId""".stripMargin)
                    
                    left[NEWSTACK, Result, X](Redirect(s"./$id"))
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
