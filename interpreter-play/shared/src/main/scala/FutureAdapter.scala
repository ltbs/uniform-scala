package ltbs.uniform
package interpreters.playframework

import cats.implicits._
import cats.data.{EitherT, RWST}
import cats.~>
import concurrent._
import play.api.mvc.Result
import common.web._

object FutureAdapter {

  def alwaysRerun(implicit ec: ExecutionContext) = new ~>[Future, WebMonad] {
    def apply[A](fa: Future[A]): WebMonad[A] =
      EitherT[WebInner, Result, A] {
        RWST { case ((_, _, _), (path, db)) =>
          fa.map{ result =>
            ((),(path,db),result.asRight)
          }
        }
      }
  }

  def rerunOnPriorStateChange(cacheId: String)(implicit ec: ExecutionContext) = new {

    def sha256Hash(in: String): String = {
      import java.security.MessageDigest
      import java.math.BigInteger
      val bytes = in.getBytes("UTF-8")
      val digest = MessageDigest.getInstance("SHA-256").digest(bytes)
      String.format("%032x", new BigInteger(1, digest))
    }

    def apply[A](fa: Future[A])(implicit codec: FormFieldEncoding[A]): WebMonad[A] =
      EitherT[WebInner, Result, A] {
        RWST { case ((_, _, _), (path, db)) =>

          val triggerValues: List[String] = path.sorted.flatMap{ db.get }
          val trigger: String = sha256Hash(triggerValues.mkString)
          val oldTrigger: Option[String] = db.get(List(cacheId, "trigger"))

          if (oldTrigger == Some(trigger)) {
            val oldValue: Either[ErrorTree,A] = 
              Input.fromUrlEncodedString(db(List(cacheId, "value"))) >>= codec.decode

            val Right(oldie) = oldValue
            Future(((),(path,db),oldie.asRight))
          } else {
            fa.map{ result =>

              val newData = Map(
                List(cacheId, "value") -> codec.encode(result).toUrlEncodedString,
                List(cacheId, "trigger") -> trigger
              )

              ((),(path,db ++ newData),result.asRight)
            }
          }
        }
      }
  }
  
}
