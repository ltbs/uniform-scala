package ltbs.uniform
package common.web

import validation.Rule

trait GenericWebInterpreter2[Html] extends MonadInterpreter[
  WebMonad[+?, Html],
  WebMonadConstructor[?, Html],
  GenericWebTell[?, Html]
] {

  def unitAsk: WebMonadConstructor[Unit, Html]
  def unitTell: GenericWebTell[Unit, Html]  

  implicit def monadInstance: cats.Monad[WebMonad[+?,Html]] =
    WebMonad.webMonadMonadInstance[Html]

  override def askImpl[A](
    key: String,
    default: Option[A],
    validation: Rule[A],
    asker: WebMonadConstructor[A,Html]
  ): WebMonad[A,Html] = asker(
    key,
    None,
    default,
    validation
  )

  override def interactImpl[A, T](
    key: String,
    tellValue: T,
    default: Option[A],
    validation: Rule[A],
    asker: WebMonadConstructor[A,Html],
    teller: GenericWebTell[T,Html]
  ): WebMonad[A, Html] =
    teller.pureHtml(tellValue, key) flatMap { t => 
      asker(
        key,
        Some(t),
        default,
        validation
      )
    }

  override def endTellImpl[T](
    key: String,
    tellValue: T,
    teller: GenericWebTell[T,Html]
  ): WebMonad[Nothing, Html] =
    teller.end(tellValue, key)

  override def endImpl(key: String): WebMonad[Nothing,Html] =
    unitTell.end((), key)

  override def tellImpl[T](
    key: String,
    tellValue: T,
    teller: GenericWebTell[T,Html]
  ): WebMonad[Unit,Html] =
    teller.pureHtml(tellValue, key).flatMap{ t => 
      unitAsk(
        key,
        Some(t), 
        None,
        Rule.alwaysPass
      )
    }
}
