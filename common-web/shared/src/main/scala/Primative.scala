package ltbs.uniform
package common.web

import scala.concurrent._

trait Primatives[Html] {

  type WM[A] = WebMonad[A, Html]
  object db {
    def updateF(dbf: DB => DB): WM[Unit] = new WebMonad[Unit, Html] {
      def apply(pageIn: PageIn[Html])(implicit ec: ExecutionContext): Future[PageOut[Unit,Html]] =
        Future.successful(
          pageIn.toPageOut(AskResult.Success[Unit,Html](())).copy(db = dbf(pageIn.state))
        )
    }

    def get[A](key: List[String])(implicit codec: Codec[A]): WM[Option[Either[ErrorTree,A]]] =
      new WebMonad[Option[Either[ErrorTree,A]], Html] {
        def apply(pageIn: PageIn[Html])(implicit ec: ExecutionContext): Future[PageOut[Option[Either[ErrorTree,A]],Html]] =
          Future.successful(
            pageIn.toPageOut(AskResult.Success(
                pageIn.state.get(key).
                  map {Input.fromUrlEncodedString(_) flatMap codec.decode}
            ))
          )
      }

    def apply[A](key: List[String])(implicit codec: Codec[A]): WM[Either[ErrorTree,A]] =
      get(key).map{_.getOrElse {Left(ErrorTree.oneErr(ErrorMsg("not-in-db")))}}

    def update[A](key: List[String], value: A)(implicit codec: Codec[A]): WM[Unit] =
      updateF{_ + (key -> codec.encode(value).toUrlEncodedString)}

    def delete(key: List[String]): WM[Unit] =
      updateF{_ - key}

    def deleteRecursive(key: List[String]): WM[Unit] =
      updateF{_.filterNot(_._1.startsWith(key))}

  }

  def goto[A](target: String): WM[A] = new WM[A] {
    def apply(pageIn: PageIn[Html])(implicit ec: ExecutionContext): Future[PageOut[A,Html]] =
      Future.successful(pageIn.toPageOut(AskResult.GotoPath[A,Html](List(target))))
  }

  protected def pushPathPrefix(key: List[String]) = new WebMonad[Unit, Html] {
    def apply(pageIn: PageIn[Html])(implicit ec: ExecutionContext): Future[PageOut[Unit,Html]] =
      Future.successful(
        pageIn.toPageOut(AskResult.Success[Unit, Html](())).copy(
          pathPrefix = pageIn.pathPrefix ++ key.toList
        )
      )
  }
  protected def popPathPrefix(qty: Int) = new WebMonad[Seq[String], Html] {
    def apply(pageIn: PageIn[Html])(implicit ec: ExecutionContext): Future[PageOut[Seq[String],Html]] =
      Future.successful(
        pageIn.toPageOut(AskResult.Success[Seq[String],Html](pageIn.pathPrefix.take(qty))) copy (
          pathPrefix = pageIn.pathPrefix.drop(qty)
        )
      )
  }
  
}
