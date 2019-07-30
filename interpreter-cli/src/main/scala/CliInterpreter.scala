package ltbs.uniform
package interpreters.cli

import shapeless._

trait TellCli[A] {
  def render(in: A): String
}

trait AskCli[A] {
  def apply(in: String, validation: List[List[Rule[A]]]): A
}

class CliInterpreter[
  SupportedTell <: HList : TypeclassList[?, TellCli],
  SupportedAsk  <: HList : TypeclassList[?, AskCli]
] extends Language[Id, SupportedTell, SupportedAsk] {

  override def interact[Tell, Ask](
    id            : String,
    t             : Tell,
    default       : Option[Ask],
    validation    : List[List[Rule[Ask]]],
    customContent : Map[String,(String,List[Any])]
  )( implicit
    selectorTell : IndexOf[SupportedTell, Tell],
    selectorAsk  : IndexOf[SupportedAsk, Ask]
  ): Ask = {
    val teller = the[TypeclassList[SupportedTell,TellCli]].
      forType[Tell].render(t)
    val asker = the[TypeclassList[SupportedAsk,AskCli]].
      forType[Ask]
    print(teller)
    asker(id, validation)
  }
}
