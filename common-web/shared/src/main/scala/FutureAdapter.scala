package ltbs.uniform
package common.web

import cats.~>
import cats.implicits._
import common.web._
import scala.concurrent.duration._
import scala.concurrent.{Future, ExecutionContext}
import java.time.LocalDateTime

object FutureAdapter {

  private def sha256Hash(in: String): String = {
    import java.security.MessageDigest
    import java.math.BigInteger
    val bytes = in.getBytes("UTF-8")
    val digest = MessageDigest.getInstance("SHA-256").digest(bytes)
    String.format("%032x", new BigInteger(1, digest))
  }

  /** Adapt any future into a web monad, use this if you're happy to
    * keep re-running the code.
    * 
    * This will be rerun very aggressively - you can expect this code
    * to execute *twice* per apparent page request (once for the POST
    * and once for the redirected GET) at every point in the journey
    * after the call to convert.
    * 
    * For this reason if your code calls a remote service or performs
    * an expensive operation you may be better using
    * rerunOnPriorStateChange.
    * 
    * This does however has the advantage that it doesn't need any
    * knowledge of the underlying datatype.
    * 
    * {{{
    * import FutureAdapter.alwaysRerun
    * 
    * interpret(myProgram(fastService)).run(targetId) { ... }
    * }}}
    */
  implicit def alwaysRerun[Html]: Future ~> WebMonad[Html, *] = new (Future ~> WebMonad[Html, *]) {
    def apply[A](fa: Future[A]): WebMonad[Html, A] = new WebMonad[Html, A] {
      def apply(pageIn: PageIn[Html])(implicit ec: ExecutionContext): Future[PageOut[Html,A]] =
        fa.map{ x => pageIn.toPageOut(AskResult.Success[Html,A](x)) }
    }
  }

  /** Adapts any future into a web monad but only re-runs if the
    * answer to a previous question in the journey has changed, or if
    * the supplied lifetime has expired. Use this if you want to
    * cache responses or otherwise avoid overwhelming a fragile
    * upstream service. 
    * 
    * Will only work if a Codec is in implicit scope for the given datatype
    * 
    * {{{
    * val adapter = FutureAdapter.rerunOnStateChange[Html](lifetime = 15.minutes)
    * import adapter._
    * 
    * interpret(myProgram(myUpstreamService)).run(targetId) { ... }
    * }}}
   */  
  case class rerunOnStateChange[Html](lifetime: Duration) {
    implicit def apply[A](
      implicit codec: Codec[A]
    ) = new Converter[Future, WebMonad[Html, *], A] {
      override def apply(cacheId: String, fa: () => Future[A]): WebMonad[Html, A] = new WebMonad[Html, A] {
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

