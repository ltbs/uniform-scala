package ltbs.uniform
package common.web

import scala.concurrent._

trait Primatives[Html] {

  type WM[A] = WebMonad[Html, A]
  object db {
    def updateF(dbf: DB => DB): WM[Unit] = new WebMonad[Html, Unit] {
      def apply(pageIn: PageIn[Html])(implicit ec: ExecutionContext): Future[PageOut[Html,Unit]] =
        Future.successful(
          pageIn.toPageOut(AskResult.Success[Html,Unit](())).copy(db = dbf(pageIn.state))
        )
    }

    def get[A](key: List[String])(implicit codec: Codec[A]): WM[Option[Either[ErrorTree,A]]] =
      new WebMonad[Html, Option[Either[ErrorTree,A]]] {
        def apply(pageIn: PageIn[Html])(implicit ec: ExecutionContext): Future[PageOut[Html,Option[Either[ErrorTree,A]]]] =
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
    def apply(pageIn: PageIn[Html])(implicit ec: ExecutionContext): Future[PageOut[Html,A]] =
      Future.successful(pageIn.toPageOut(AskResult.GotoPath[Html,A](List(target))))
  }

  protected def pushPathPrefix(key: List[String]) = new WebMonad[Html, Unit] {
    def apply(pageIn: PageIn[Html])(implicit ec: ExecutionContext): Future[PageOut[Html,Unit]] =
      Future.successful(
        pageIn.toPageOut(AskResult.Success[Html, Unit](())).copy(
          pathPrefix = pageIn.pathPrefix ++ key.toList
        )
      )
  }
  protected def popPathPrefix(qty: Int) = new WebMonad[Html, Seq[String]] {
    def apply(pageIn: PageIn[Html])(implicit ec: ExecutionContext): Future[PageOut[Html, Seq[String]]] =
      Future.successful(
        pageIn.toPageOut(AskResult.Success[Html, Seq[String]](pageIn.pathPrefix.take(qty))) copy (
          pathPrefix = pageIn.pathPrefix.drop(qty)
        )
      )
  }

  def pushBreadcrumb(bc: List[String]) = new WebMonad[Html, Unit] {
    override def apply(pageIn: PageIn[Html])(implicit ec: ExecutionContext): Future[PageOut[Html, Unit]] =
      Future.successful(
        pageIn.toPageOut(AskResult.Success[Html, Unit](())) copy (
            breadcrumbs = bc :: pageIn.breadcrumbs
          )
      )
  }

  def putConfig(in: JourneyConfig) = new WebMonad[Html, Unit] {
    override def apply(pageIn: PageIn[Html])(implicit ec: ExecutionContext): Future[PageOut[Html, Unit]] =
      Future.successful(
        pageIn.toPageOut(AskResult.Success[Html, Unit](())) copy (
           config = in
        )
      )
  }

  def getConfig = new WebMonad[Html, JourneyConfig] {
    override def apply(pageIn: PageIn[Html])(implicit ec: ExecutionContext): Future[PageOut[Html, JourneyConfig]] =
      Future.successful(
        pageIn.toPageOut(AskResult.Success[Html, JourneyConfig](pageIn.config))
      )
  }

  
  def subjourneyWM[B](
    configModifier: JourneyConfig => JourneyConfig,
    path: String*
  )(
    inner: WebMonad[Html, B]
  ): WebMonad[Html, B] = {
    for {
      origConf <- getConfig
      _        <- putConfig(configModifier(origConf))
      _        <- pushPathPrefix(path.toList)
      result   <- inner
      _        <- popPathPrefix(path.size)
      _        <- putConfig(origConf)
    } yield result
  }

}
