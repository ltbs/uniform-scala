package ltbs.uniform
package common.web

import cats.Monad
import cats.implicits._
import concurrent._

abstract class WebMonad[Html, +A] {
  def apply(pageIn: PageIn[Html])(implicit ec: ExecutionContext): Future[PageOut[Html,A]]

  def flatMap[B](f: A => WebMonad[Html,B]): WebMonad[Html,B] = {
    val fa = this
    new WebMonad[Html,B] {
      def apply(pageIn: PageIn[Html])(implicit ec: ExecutionContext): Future[PageOut[Html,B]] = {
        fa.apply(pageIn).flatMap[PageOut[Html,B]] { _ match {
          case PageOut(p,db,AskResult.Success(a), pp, conf) =>
            f(a).apply(pageIn.copy(state = db, breadcrumbs = p, pathPrefix = pp, config = conf))
          case PageOut(p,db,gp: AskResult.GotoPath[Html, A], pp, conf) =>
            PageOut(p,db,gp.map[B], pathPrefix = pp, config = conf).pure[Future]
          case PageOut(p,db,pl: AskResult.Payload[Html, A], pp, conf) =>
            PageOut(p,db,pl.map[B], pathPrefix = pp, config = conf).pure[Future]
        } }
      }
    }
  }

  def map[B](f: A => B): WebMonad[Html,B] =
    WebMonad.webMonadMonadInstance.map(this)(f)
}

object WebMonad {

  def pure[Html, A](x: A): WebMonad[Html,A] = new WebMonad[Html, A] {
    def apply(pageIn: PageIn[Html])(implicit ec: ExecutionContext): Future[PageOut[Html,A]] = {
      import pageIn._
      PageOut(breadcrumbs,state,AskResult.Success[Html,A](x), pageIn.pathPrefix, pageIn.config).pure[Future]
    }
  }

  implicit def webMonadMonadInstance[Html] =
    new Monad[WebMonad[Html, +?]] {

      def pure[A](x: A): WebMonad[Html,A] = WebMonad.pure(x)

      def flatMap[A, B](fa: WebMonad[Html,A])(f: A => WebMonad[Html,B]): WebMonad[Html,B] =
        fa.flatMap(f)

      // may not be stack-safe
      def tailRecM[A, B](a: A)(f: A => WebMonad[Html, Either[A, B]]): WebMonad[Html, B] = flatMap(f(a)) {
        case Left(a)  => tailRecM(a)(f)
        case Right(b) => pure(b)
      }
    }

}
