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
  ) = new Language[WebMonad[?,Html], SupportedTell, SupportedAsk]{

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
  }

  def goto[A](target: String): WM[A] = new WM[A] {
    def apply(pageIn: PageIn)(implicit ec: ExecutionContext): Future[PageOut[A,Html]] =
      Future.successful(
        PageOut[A,Html](
          path = pageIn.path,
          db = pageIn.state,
          output = AskResult.GotoPath(List(target))
        )
      )
  }

  object db {

    def updateF(dbf: DB => DB): WM[Unit] = new WebMonad[Unit, Html] {
      def apply(pageIn: PageIn)(implicit ec: ExecutionContext): Future[PageOut[Unit,Html]] =
        Future.successful(
          PageOut[Unit,Html](
            path = pageIn.path,
            db = dbf(pageIn.state),
            output = AskResult.Success(())
          )
        )
    }

    def get[A](key: List[String])(implicit codec: Codec[A]): WM[Option[Either[ErrorTree,A]]] =
      new WebMonad[Option[Either[ErrorTree,A]], Html] {
        def apply(pageIn: PageIn)(implicit ec: ExecutionContext): Future[PageOut[Option[Either[ErrorTree,A]],Html]] =
          Future.successful(
            PageOut[Option[Either[ErrorTree,A]],Html](
              path = pageIn.path,
              db = pageIn.state,
              output = AskResult.Success(
                pageIn.state.get(key).
                  map {Input.fromUrlEncodedString(_) flatMap codec.decode}
              )
            )
          )
      }

    def apply[A](key: List[String])(implicit codec: Codec[A]): WM[Either[ErrorTree,A]] =
      get(key).map{_.getOrElse {Left(ErrorTree.oneErr(ErrorMsg("not-in-db")))}}

    def update[A](key: List[String], value: A)(implicit codec: Codec[A]): WM[Unit] =
      updateF{_ + (key -> codec.encode(value).toUrlEncodedString)}
    def delete(key: List[String]): WM[Unit] =
      updateF{_ - key}
  }

}
