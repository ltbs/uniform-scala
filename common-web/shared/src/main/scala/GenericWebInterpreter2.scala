package ltbs.uniform
package common.web

import validation.Rule

class GenericWebInterpreter2[Html](
  empty: Html, 
  messages: UniformMessages[Html],
  unitAsk: WebMonadConstructor[Unit, Html]
) extends MonadInterpreter[
  WebMonad[+?, Html],
  WebMonadConstructor[?, Html],
  GenericWebTell[?, Html]
] {

  def ask[A](
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

  override def interact[A, T](
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

  override def endTell[T](
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

  def end(key: String): WebMonad[Nothing,Html] = 
    WebMonad.webMonadMonadInstance.map(
      unitAsk(
        key,
        empty,
        None,
        Rule.alwaysFail,
        messages
      )
    )(_ => throw new IllegalStateException("unable to continue - end of journey"))

  def tell[T](
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
