package ltbs.uniform
package common.web

import validation.Rule

trait GenericWebInterpreter2[Html] extends MonadInterpreter[
  WebMonad[+?, Html],
  WebInteraction[?, Html],
  GenericWebTell[?, Html]
] {

  def unitAsk: WebInteraction[Unit, Html]
  def unitTell: GenericWebTell[Unit, Html]  

  implicit def monadInstance: cats.Monad[WebMonad[+?,Html]] =
    WebMonad.webMonadMonadInstance[Html]

  override def askImpl[A](
    key: String,
    default: Option[A],
    validation: Rule[A],
    customContent: Map[String,(String,List[Any])],    
    asker: WebInteraction[A,Html]
  ): WebMonad[A,Html] = asker(
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
    asker: WebInteraction[A,Html],
    teller: GenericWebTell[T,Html]
  ): WebMonad[A, Html] =
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
    teller: GenericWebTell[T,Html]
  ): WebMonad[Nothing, Html] =
    teller.end(tellValue, key, customContent)

  override def endImpl(
    key: String,
    customContent: Map[String,(String,List[Any])],    
  ): WebMonad[Nothing,Html] =
    unitTell.end((), key, customContent)

  override def tellImpl[T](
    key: String,
    tellValue: T,
    customContent: Map[String,(String,List[Any])],    
    teller: GenericWebTell[T,Html]
  ): WebMonad[Unit,Html] =
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
}
