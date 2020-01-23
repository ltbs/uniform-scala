package ltbs.uniform
package common.web

import shapeless._
import scala.concurrent._
import cats.implicits._
import validation.Rule

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
      validationIn: Rule[Ask],
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

    override def subJourney[A](id: String*)(sub: => WM[A]): WM[A] =
      genericSubJourney[A](id)(sub)
  }

  def genericSubJourney[A](
    id: Seq[String],
    config: JourneyConfig => JourneyConfig = identity
  )(sub: => WM[A]): WM[A] = {
    import cats.implicits._
    println(s"####### JourneyConfig is $config")
    getConfig() >>= {origConf => mutateConfig(config) >> pushPathPrefix(id) >> sub <* popPathPrefix(id.size) <* mutateConfig{_ => origConf}}
  }

  def pushPathPrefix(key: Seq[String]) = new WM[Unit] {
    def apply(pageIn: PageIn)(implicit ec: ExecutionContext): Future[PageOut[Unit,Html]] =
      Future.successful(
        pageIn.toPageOut(AskResult.Success[Unit, Html](())).copy(
          pathPrefix = pageIn.pathPrefix ++ key.toList
        )
      )
  }

  def mutateConfig(f: JourneyConfig => JourneyConfig) = new WM[Unit] {
    def apply(pageIn: PageIn)(implicit ec: ExecutionContext): Future[PageOut[Unit,Html]] =
      Future.successful(
        pageIn.toPageOut(AskResult.Success[Unit, Html](())).copy(
          config = f(pageIn.config)
        )
      )
  }

  def getConfig() = new WM[JourneyConfig] {
    def apply(pageIn: PageIn)(implicit ec: ExecutionContext): Future[PageOut[JourneyConfig,Html]] =
      Future.successful(
        pageIn.toPageOut(AskResult.Success[JourneyConfig, Html](pageIn.config))
      )
  }

  def popPathPrefix(qty: Int) = new WM[Seq[String]] {
    def apply(pageIn: PageIn)(implicit ec: ExecutionContext): Future[PageOut[Seq[String],Html]] =
      Future.successful(
        pageIn.toPageOut(AskResult.Success[Seq[String],Html](pageIn.pathPrefix.take(qty))) copy (
          pathPrefix = pageIn.pathPrefix.drop(qty)
        )
      )
  }

  def goto[A](target: String): WM[A] = new WM[A] {
    def apply(pageIn: PageIn)(implicit ec: ExecutionContext): Future[PageOut[A,Html]] =
      Future.successful(pageIn.toPageOut(AskResult.GotoPath[A,Html](List(target))))
  }

  object db {
    def updateF(dbf: DB => DB): WM[Unit] = new WebMonad[Unit, Html] {
      def apply(pageIn: PageIn)(implicit ec: ExecutionContext): Future[PageOut[Unit,Html]] =
        Future.successful(
          pageIn.toPageOut(AskResult.Success[Unit,Html](())).copy(db = dbf(pageIn.state))
        )
    }

    def get[A](key: List[String])(implicit codec: Codec[A]): WM[Option[Either[ErrorTree,A]]] =
      new WebMonad[Option[Either[ErrorTree,A]], Html] {
        def apply(pageIn: PageIn)(implicit ec: ExecutionContext): Future[PageOut[Option[Either[ErrorTree,A]],Html]] =
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

}
