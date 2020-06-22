package ltbs.uniform
package common.web

import validation.Rule

trait GenericWebInterpreter2[Html] extends MonadInterpreter[
  WebMonad[+?, Html],
  WebMonadConstructor[?, Html],
  GenericWebTell[?, Html]
] {

  def empty: Html
  def messages: UniformMessages[Html]
  def unitAsk: WebMonadConstructor[Unit, Html]

  implicit def monadInstance: cats.Monad[WebMonad[?,Html]] =
    WebMonad.webMonadMonadInstance[Html]

  override def askImpl[A](
    key: String,
    default: Option[A],
    validation: Rule[A],
    asker: WebMonadConstructor[A,Html]
  ): WebMonad[A,Html] = asker(
    key,
    empty,
    default,
    validation,
    messages
  )

  override def interactImpl[A, T](
    key: String,
    tellValue: T,
    default: Option[A],
    validation: Rule[A],
    asker: WebMonadConstructor[A,Html],
    teller: GenericWebTell[T,Html]
  ): WebMonad[A, Html] = asker(
    key,
    teller.render(tellValue, key, messages),
    default,
    validation,
    messages
  )

  override def endTellImpl[T](
    key: String,
    value: T,
    teller: GenericWebTell[T,Html]
  ): WebMonad[Nothing, Html] =
    WebMonad.webMonadMonadInstance.map(
      unitAsk(
        key,
        teller.render(value, key, messages),
        None,
        Rule.alwaysFail,
        messages
      )
    )(_ => throw new IllegalStateException("unable to continue - end of journey"))

  override def endImpl(key: String): WebMonad[Nothing,Html] =
    WebMonad.webMonadMonadInstance.map(
      unitAsk(
        key,
        empty,
        None,
        Rule.alwaysFail,
        messages
      )
    )(_ => throw new IllegalStateException("unable to continue - end of journey"))

  override def tellImpl[T](
    key: String,
    value: T,
    teller: GenericWebTell[T,Html]
  ): WebMonad[Unit,Html] = unitAsk(
    key,
    teller.render(value, key, messages),
    None,
    Rule.alwaysPass,
    messages
  )
  
}
