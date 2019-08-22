package ltbs.uniform
package common.web

import shapeless._
import scala.concurrent._
import cats.syntax.functor._

trait GenericWebInterpreter[Html] {

  type WebTell[A] = GenericWebTell[A, Html]
  type WMC[A] = WebMonadConstructor[A, Html]
  type WM[A] = WebMonad[A, Html]

  def create[
    SupportedTell <: HList,
    SupportedAsk  <: HList
  ](messages: UniformMessages[Html])(
    implicit tellSummoner : TypeclassList[SupportedTell, WebTell],
    webMonadSummoner      : TypeclassList[SupportedAsk, WMC]
  ) = new Language[WM, SupportedTell, SupportedAsk]{

    def interact[Tell, Ask](
      id: String,
      tell: Tell,
      defaultIn: Option[Ask],
      validationIn: List[List[Rule[Ask]]],
      customContent: Map[String,(String, List[Any])]
    )(implicit
      selectorTell: IndexOf[SupportedTell,Tell],
      selectorAsk: IndexOf[SupportedAsk,Ask]
    ): WebMonad[Ask,Html] = {
      val customMessages = messages withCustomContent customContent
      val tellHtml = tellSummoner.forType[Tell].render(tell, id, customMessages)
      webMonadSummoner.forType[Ask].apply(
        id,
        tellHtml,
        defaultIn,
        validationIn,
        customMessages
      )
    }

    override def subJourney[A](id: String)(sub: => WM[A]): WM[A] =
      genericSubJourney[A](id)(sub)
  }

  def genericSubJourney[A](id: String)(sub: => WM[A]): WM[A] = {
    import cats.implicits._
    pushPathPrefix(id) >> sub <* popPathPrefix
  }

  def pushPathPrefix(key: String) = new WM[Unit] {
    def apply(pageIn: PageIn)(implicit ec: ExecutionContext): Future[PageOut[Unit,Html]] =
      Future.successful(
        pageIn.toPageOut(AskResult.Success[Unit, Html](())).copy(
          pathPrefix = key :: pageIn.pathPrefix
        )
      )
  }

  def popPathPrefix = new WM[String] {
    def apply(pageIn: PageIn)(implicit ec: ExecutionContext): Future[PageOut[String,Html]] =
      Future.successful(
        PageOut[String,Html](
          breadcrumbs = pageIn.breadcrumbs,
          db = pageIn.state,
          output = AskResult.Success(pageIn.pathPrefix.head),
          pathPrefix = pageIn.pathPrefix.tail
        )
      )
  }

  def goto[A](target: String): WM[A] = new WM[A] {
    def apply(pageIn: PageIn)(implicit ec: ExecutionContext): Future[PageOut[A,Html]] =
      Future.successful(
        PageOut[A,Html](
          breadcrumbs = pageIn.breadcrumbs,
          db = pageIn.state,
          output = AskResult.GotoPath(List(target)),
          pathPrefix = pageIn.pathPrefix
        )
      )
  }

  object db {

    def updateF(dbf: DB => DB): WM[Unit] = new WebMonad[Unit, Html] {
      def apply(pageIn: PageIn)(implicit ec: ExecutionContext): Future[PageOut[Unit,Html]] =
        Future.successful(
          PageOut[Unit,Html](
            breadcrumbs = pageIn.breadcrumbs,
            db = dbf(pageIn.state),
            output = AskResult.Success(()),
            pathPrefix = pageIn.pathPrefix
          )
        )
    }

    def get[A](key: List[String])(implicit codec: Codec[A]): WM[Option[Either[ErrorTree,A]]] =
      new WebMonad[Option[Either[ErrorTree,A]], Html] {
        def apply(pageIn: PageIn)(implicit ec: ExecutionContext): Future[PageOut[Option[Either[ErrorTree,A]],Html]] =
          Future.successful(
            PageOut[Option[Either[ErrorTree,A]],Html](
              breadcrumbs = pageIn.breadcrumbs,
              db = pageIn.state,
              output = AskResult.Success(
                pageIn.state.get(key).
                  map {Input.fromUrlEncodedString(_) flatMap codec.decode}
              ),
              pathPrefix = pageIn.pathPrefix
            )
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

}
