package ltbs.uniform
package common.web

import validation.Rule

trait WebInterpreter[Html] extends Primatives[Html] with MonadInterpreter [
  WebMonad[Html, +?],
  WebInteraction[Html, ?, ?],
  WebAskList[Html, ?]
] {

  implicit def monadInstance: cats.Monad[WebMonad[Html, +?]] =
    WebMonad.webMonadMonadInstance[Html]

  override def interactImpl[T, A](
    key: String,
    tellValue: T,
    default: Option[A],
    validation: Rule[A],
    customContent: Map[String,(String,List[Any])],
    wa: WebInteraction[Html,T,A]
  ): WebMonad[Html, A] = wa(
    key,
    Some(tellValue),
    default,
    validation,
    customContent
  )

  override def subjourneyImpl[A](
    path: List[String],
    inner: WebMonad[Html, A]
  ): WebMonad[Html, A] = subjourneyWM(identity, path:_*)(inner)

  override def askListImpl[A](
    key: String,
    askJourney: (Option[Int], List[A]) => WebMonad[Html,A],
    default: Option[List[A]],
    validation: Rule[List[A]],
    customContent: Map[String,(String, List[Any])],
    asker: WebAskList[Html,A]
  ): WebMonad[Html, List[A]] =
    asker(key, askJourney, default, validation, customContent)

  override def nonReturnImpl(
    nonReturn: Uniform.NonReturn
  ): WebMonad[Html, Unit] = for {
    pp <- getPathPrefix
    fullKey = pp :+ nonReturn.key
    _  <- db.updateF{db => db + (("_non-return" :: Nil) -> fullKey.mkString("/"))}
    _  <- pushBreadcrumb(fullKey.toList)
  } yield ()

  implicit def unitField = new WebAsk[Html,Unit] {
    def decode(out: Input): Either[ltbs.uniform.ErrorTree,Unit] = Right(())
    def encode(in: Unit): Input = Input.empty
    def render(
      pageIn: PageIn[Html],
      pageKey: List[String],
      fieldKey: List[String],
      tell: Option[Html],
      data: Input,
      errors: ErrorTree
    ): Option[Html] = tell
  }

  implicit def nothingField = new WebAsk[Html,Nothing] {
    def decode(out: Input): Either[ltbs.uniform.ErrorTree,Nothing] =
      Left(ErrorMsg("tried to decode to nothing").toTree)

    def encode(in: Nothing): Input =
      sys.error("encoding nothing is not possible!")

    def render(
      pageIn: PageIn[Html],      
      pageKey: List[String],
      fieldKey: List[String],
      tell: Option[Html],
      data: Input,
      errors: ErrorTree
    ): Option[Html] = tell
  }

  implicit val tellHtml = new WebTell[Html, Html] {
    def render(
      in: Html,
      key: String,
      messages: UniformMessages[Html]
    ): Option[Html] = Some(in)
  }

}
