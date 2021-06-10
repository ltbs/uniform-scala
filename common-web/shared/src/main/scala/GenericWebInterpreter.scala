package ltbs.uniform
package common.web

import validation.Rule

trait GenericWebInterpreter[Html] extends Primatives[Html] with MonadInterpreter [
  WebMonad[Html, +?],
  GenericWebTell[Html, ?],  
  WebInteraction[Html, ?],
  WebAskList[Html, ?]
] {

  def unitAsk: WebInteraction[Html, Unit]
  def unitTell: GenericWebTell[Html, Unit]  

  implicit def monadInstance: cats.Monad[WebMonad[Html, +?]] =
    WebMonad.webMonadMonadInstance[Html]

  override def askImpl[A](
    key: String,
    default: Option[A],
    validation: Rule[A],
    customContent: Map[String,(String,List[Any])],    
    asker: WebInteraction[Html,A]
  ): WebMonad[Html,A] = asker(
    key,
    None,
    default,
    validation,
    customContent
  )

  override def interactImpl[A, T](
    key: String,
    tellValue: T,
    default: Option[A],
    validation: Rule[A],
    customContent: Map[String,(String,List[Any])],    
    asker: WebInteraction[Html,A],
    teller: GenericWebTell[Html,T]
  ): WebMonad[Html, A] =
    teller.pureHtml(tellValue, key, customContent) flatMap { t => 
      asker(
        key,
        Some(t),
        default,
        validation,
        customContent
      )
    }

  override def endTellImpl[T](
    key: String,
    tellValue: T,
    customContent: Map[String,(String,List[Any])],    
    teller: GenericWebTell[Html,T]
  ): WebMonad[Html, Nothing] =
    teller.end(tellValue, key, customContent)

  override def endImpl(
    key: String,
    customContent: Map[String,(String,List[Any])],    
  ): WebMonad[Html,Nothing] =
    unitTell.end((), key, customContent)

  override def tellImpl[T](
    key: String,
    tellValue: T,
    customContent: Map[String,(String,List[Any])],    
    teller: GenericWebTell[Html,T]
  ): WebMonad[Html,Unit] =
    teller.pureHtml(
      tellValue,
      key,
      customContent
    ) flatMap { t =>
      unitAsk(
        key,
        Some(t), 
        None,
        Rule.alwaysPass,
        customContent
      )
    }

  override def subjourneyImpl[A](
    path: List[String],
    inner: WebMonad[Html, A]
  ): WebMonad[Html, A] = {
    for {
      _      <- pushPathPrefix(path)
      result <- inner
      _      <- popPathPrefix(path.size)
    } yield result
  }

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
