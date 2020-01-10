package controllers

import cats.implicits._
import cats.~>
import javax.inject._
import ltbs.uniform._, interpreters.playframework._, examples.dst._
import common.web.{WebMonad, FutureAdapter}
import play.api.i18n.{Messages => _, _}
import play.api.mvc._
import scala.concurrent._
import scalatags.Text.all._

case class BackendServiceFutureImpl(implicit ec: ExecutionContext) extends BackendService[Future] {

  def matchedCompany(): Future[Option[Company]] =
    Future.successful(None)          

  def lookup(utr: String, postcode: String): Future[Option[Company]] =
    Future.successful(Some(Company("testco", Address("a","b","c","postcode"))))    

}

@Singleton
class DstController @Inject()(
  implicit val messagesApi: MessagesApi,
  ec: ExecutionContext
) extends ControllerHelpers with I18nSupport {

  implicit val persistence: PersistenceEngine[Request[AnyContent]] =
    UnsafePersistence()

  val hod = BackendServiceFutureImpl().natTransform[WebMonad[?, Tag]](
    new (Future ~> WebMonad[?, Tag]) {
      def apply[A](in: Future[A]): WebMonad[A, Tag] = FutureAdapter[Tag].alwaysRerun(in)
    }
  )

  lazy val interpreter = HmrcPlayInterpreter(this, messagesApi)

  def register(targetId: String) = Action.async { implicit request: Request[AnyContent] =>
    import interpreter._

    val playProgram = registrationJourney[interpreter.WM](
      create[RegTellTypes, RegAskTypes](interpreter.messages(request)),
      hod
    )

    playProgram.run(targetId, purgeStateUponCompletion = true) {
      i: Registration => Ok(s"$i").pure[Future]
    }
  }

  def submitReturn(targetId: String) = Action.async { implicit request: Request[AnyContent] =>
    import interpreter._

    val playProgram = returnJourney[interpreter.WM](
      create[ReturnTellTypes, ReturnAskTypes](interpreter.messages(request)),
      hod
    )

    playProgram.run(targetId, purgeStateUponCompletion = true) {
      i: Return => Ok(s"$i").pure[Future]
    }
  }



}

