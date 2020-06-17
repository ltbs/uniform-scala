package ltbs.uniform
package common.web

import cats.Monad
import cats.implicits._
import concurrent._

abstract class WebMonad[+A,Html] {
  def apply(pageIn: PageIn)(implicit ec: ExecutionContext): Future[PageOut[A,Html]]
}

object WebMonad {

  implicit def webMonadMonadInstance[Html] =
    new Monad[WebMonad[?, Html]] {

      def pure[A](x: A): WebMonad[A,Html] = new WebMonad[A, Html] {
        def apply(pageIn: PageIn)(implicit ec: ExecutionContext): Future[PageOut[A,Html]] = {
          import pageIn._
          PageOut(breadcrumbs,state,AskResult.Success[A,Html](x), pageIn.pathPrefix, pageIn.config).pure[Future]
        }
      }

      def flatMap[A, B](fa: WebMonad[A,Html])(f: A => WebMonad[B,Html]): WebMonad[B,Html] = {
        new WebMonad[B,Html] {
          def apply(pageIn: PageIn)(implicit ec: ExecutionContext): Future[PageOut[B,Html]] = {
            fa.apply(pageIn).flatMap[PageOut[B,Html]] { _ match {
              case PageOut(p,db,AskResult.Success(a), pp, _) =>
                f(a).apply(pageIn.copy(state = db, breadcrumbs = p, pathPrefix = pp))
              case PageOut(p,db,gp: AskResult.GotoPath[A, Html], pp, conf) =>
                PageOut(p,db,gp.map[B], pathPrefix = pp, config = conf).pure[Future]
              case PageOut(p,db,pl: AskResult.Payload[A, Html], pp, conf) =>
                PageOut(p,db,pl.map[B], pathPrefix = pp, config = conf).pure[Future]
            } }
          }
        }
      }

      // may not be stack-safe
      def tailRecM[A, B](a: A)(f: A => WebMonad[Either[A, B], Html]): WebMonad[B, Html] = flatMap(f(a)) {
        case Left(a)  => tailRecM(a)(f)
        case Right(b) => pure(b)
      }
    }
}
