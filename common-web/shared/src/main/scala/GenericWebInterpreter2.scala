package ltbs.uniform
package common.web

import validation.Rule

trait GenericWebInterpreter2[Html] extends Primatives[Html] with MonadInterpreter [
  WebMonad[Html, +?],
  WebInteraction[Html, ?, ?],
  WebAskList[Html, ?]
] {

  implicit def createInteraction[T,A](
    implicit tell: GenericWebTell[Html, T],
    ask: FormField[Html, A]
  ): WebInteraction[Html, T,A] = new StandardTellAndAskForm(tell, ask)

//  def unitInteraction: WebInteraction[Html, Unit, Unit]

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
  ): WebMonad[Html, A] = {
    for {
      _      <- pushPathPrefix(path)
      result <- inner
      _      <- popPathPrefix(path.size)
    } yield result
  } // why is there no << defined, even in haskell?
  // I suspect we can't use <* here as we don't want to discard the monadic side-effect

  override def askListImpl[A](
    key: String,
    askJourney: (Option[Int], List[A]) => WebMonad[Html,A],
    default: Option[List[A]],
    validation: Rule[List[A]],
    customContent: Map[String,(String, List[Any])],
    asker: WebAskList[Html,A]
  ): WebMonad[Html, List[A]] =
    asker(key, askJourney, default, validation, customContent)

}
