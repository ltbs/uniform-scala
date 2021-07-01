package controllers

import cats.implicits._
import javax.inject._
import ltbs.uniform._, interpreters.playframework._, examples.beardtax._
import play.api.i18n.{Messages => _, _}
import play.api.mvc._
import scala.concurrent.{Future, ExecutionContext}
import scalatags.Text.all._
import cats.~>
import ltbs.uniform.common.web._
import java.time.LocalDateTime
import scala.concurrent.duration._

class HodConnector(implicit ec: ExecutionContext) extends Hod[Future] {

  def recordBeardHeight(height: Int): Future[Unit] = Future{
    println(s"HOD CALL: $height")
  }

  def costOfBeard(beardStyle: BeardStyle, length: BeardLength): Future[Int] =
    Future{
      Thread.sleep(2000)
      IdDummyHod.costOfBeard(beardStyle, length)
    }
}

@Singleton
class BeardController2 @Inject()(
  implicit ec:ExecutionContext,
  val controllerComponents: ControllerComponents
) extends BaseController with ControllerHelpers with I18nSupport with HmrcPlayInterpreter {

  implicit val persistence: PersistenceEngine[Request[AnyContent]] =
    DebugPersistence(UnsafePersistence())

  def beardAction(targetId: String) = Action.async { implicit request: Request[AnyContent] =>

    implicit val persistence: PersistenceEngine[Request[AnyContent]] =
      SessionPersistence("beard")

    object FutureAdaptorZ {

      def sha256Hash(in: String): String = {
        import java.security.MessageDigest
        import java.math.BigInteger
        val bytes = in.getBytes("UTF-8")
        val digest = MessageDigest.getInstance("SHA-256").digest(bytes)
        String.format("%032x", new BigInteger(1, digest))
      }

      // an example of how to adapt any future into a web monad, use this if you're happy to
      // keep re-running the code on every page hit after the call to convert.
      implicit def alwaysRerun: Future ~> WM = new (Future ~> WM) {
        def apply[A](fa: Future[A]): WM[A] = new WM[A] {
          def apply(pageIn: PageIn[Tag])(implicit ec: ExecutionContext): Future[PageOut[Tag,A]] =
            fa.map{ x => pageIn.toPageOut(AskResult.Success[Tag,A](x)) }
        }
      }

      // an example of how to adapt any future for a specific type into a web monad, for example
      // if you want to use a codec to facilitate caching
      //
      // For some reason this doesn't work at present (triggers a strange error at runtime)
      implicit def rerunOnPriorStateChange[A](implicit codec: ltbs.uniform.common.web.Codec[A]) = new Converter[Future, WM, A] {

        val lifetime: Duration = 1.minute // needs to be configurable

        override def apply(cacheId: String, fa: () => Future[A]): WM[A] = new WM[A] {
          def apply(pageIn: PageIn[Tag])(implicit ec: ExecutionContext): Future[PageOut[Tag,A]] = {
            import pageIn._
            val triggerValues: List[String] = breadcrumbs.sorted.flatMap{ state.get }
            val trigger: String = sha256Hash(triggerValues.mkString)
            val oldTrigger: Option[String] = state.get(List(cacheId, "trigger"))
            val timestamp: Option[LocalDateTime] = state
              .get(List(cacheId, "timestamp"))
              .map{LocalDateTime.parse}
            if (oldTrigger == Some(trigger) && timestamp.fold(false)(_ + lifetime > LocalDateTime.now)) {
              val oldValue: Either[ErrorTree,A] =
                Input.fromUrlEncodedString(state(List(cacheId, "value")).tail) >>= codec.decode

              val Right(oldie) = oldValue
              pageIn.toPageOut(AskResult.Success[Tag, A](oldie)).pure[Future]
            } else {
              fa().map{ result =>
                val value = codec.encode(result)
                val newData = Map(
                  List(cacheId, "value") -> ("V" + value.toUrlEncodedString),
                  List(cacheId, "trigger") -> trigger,
                  List(cacheId, "timestamp") -> LocalDateTime.now.toString
                )
                pageIn.toPageOut(AskResult.Success[Tag, A](result)).copy (
                  db = pageIn.state ++ newData
                )
              }
            }
          }
        }
      }
    }

    import FutureAdaptorZ.rerunOnPriorStateChange

    interpret(beardProgram(new HodConnector)).runSync(targetId){ 
      i: Int => Ok(s"$i")
    }
  }
}


