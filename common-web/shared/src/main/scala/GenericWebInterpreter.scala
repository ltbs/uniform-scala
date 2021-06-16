package ltbs.uniform
package common.web

import validation.Rule
import cats.~>
import scala.concurrent.{Future, ExecutionContext}
import scala.concurrent.duration._
import cats.implicits._
import java.time.LocalDateTime

trait GenericWebInterpreter[Html] extends Primatives[Html] with MonadInterpreter [
  WebMonad[Html, +?],
  GenericWebTell[Html, ?],  
  WebInteraction[Html, ?],
  WebAskList[Html, ?]
] {

  def unitAsk: WebInteraction[Html, Unit]
  def unitTell: GenericWebTell[Html, Unit]  

  implicit def monadInstance: cats.Monad[WebMonad[Html, +?]] =
    WebMonad.webMonadMonadInstance[Html]

  override def askImpl[A](
    key: String,
    default: Option[A],
    validation: Rule[A],
    customContent: Map[String,(String,List[Any])],    
    asker: WebInteraction[Html,A]
  ): WebMonad[Html,A] = asker(
    key,
    None,
    default,
    validation,
    customContent
  )

  override def interactImpl[A, T](
    key: String,
    tellValue: T,
    default: Option[A],
    validation: Rule[A],
    customContent: Map[String,(String,List[Any])],    
    asker: WebInteraction[Html,A],
    teller: GenericWebTell[Html,T]
  ): WebMonad[Html, A] =
    teller.pureHtml(tellValue, key, customContent) flatMap { t => 
      asker(
        key,
        Some(t),
        default,
        validation,
        customContent
      )
    }

  override def endTellImpl[T](
    key: String,
    tellValue: T,
    customContent: Map[String,(String,List[Any])],    
    teller: GenericWebTell[Html,T]
  ): WebMonad[Html, Nothing] =
    teller.end(tellValue, key, customContent)

  override def endImpl(
    key: String,
    customContent: Map[String,(String,List[Any])],    
  ): WebMonad[Html,Nothing] =
    unitTell.end((), key, customContent)

  override def tellImpl[T](
    key: String,
    tellValue: T,
    customContent: Map[String,(String,List[Any])],    
    teller: GenericWebTell[Html,T]
  ): WebMonad[Html,Unit] =
    teller.pureHtml(
      tellValue,
      key,
      customContent
    ) flatMap { t =>
      unitAsk(
        key,
        Some(t), 
        None,
        Rule.alwaysPass,
        customContent
      )
    }

  override def subjourneyImpl[A](
    path: List[String],
    inner: WebMonad[Html, A]
  ): WebMonad[Html, A] = {
    for {
      _      <- pushPathPrefix(path)
      result <- inner
      _      <- popPathPrefix(path.size)
    } yield result
  }

  override def askListImpl[A](
    key: String,
    askJourney: (Option[Int], List[A]) => WebMonad[Html,A],
    default: Option[List[A]],
    validation: Rule[List[A]],
    customContent: Map[String,(String, List[Any])],
    asker: WebAskList[Html,A]
  ): WebMonad[Html, List[A]] =
    asker(key, askJourney, default, validation, customContent)

  object FutureAdaptor {

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
        def apply(pageIn: PageIn[Html])(implicit ec: ExecutionContext): Future[PageOut[Html,A]] =
          fa.map{ x => pageIn.toPageOut(AskResult.Success[Html,A](x)) }
      }
    }

    // an example of how to adapt any future for a specific type into a web monad, for example
    // if you want to use a codec to facilitate caching
    //
    // For some reason this doesn't work at present (triggers a strange error at runtime)
    implicit def rerunOnPriorStateChange[A](implicit codec: ltbs.uniform.common.web.Codec[A]) = new Converter[Future, WM, A] {

      val lifetime: Duration = 1.minute // needs to be configurable

      override def apply(cacheId: String, fa: () => Future[A]): WM[A] = new WM[A] {
        def apply(pageIn: PageIn[Html])(implicit ec: ExecutionContext): Future[PageOut[Html,A]] = {
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
            pageIn.toPageOut(AskResult.Success[Html, A](oldie)).pure[Future]
          } else {
            fa().map{ result =>
              val value = codec.encode(result)
              val newData = Map(
                List(cacheId, "value") -> ("V" + value.toUrlEncodedString),
                List(cacheId, "trigger") -> trigger,
                List(cacheId, "timestamp") -> LocalDateTime.now.toString
              )
              pageIn.toPageOut(AskResult.Success[Html, A](result)).copy (
                db = pageIn.state ++ newData
              )
            }
          }
        }
      }
    }
  }

}
