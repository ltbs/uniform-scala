package ltbs.uniform
package interpreter.asterisk

//import cats.implicits._
//import language.higherKinds
import javax.sound.sampled.AudioInputStream
import concurrent.Future
import shapeless._

trait TellAsterisk[A] {
  def say(key: String, in: A): AudioInputStream
}

trait AskAsterisk[A] {
  def prompt(key: String): AudioInputStream
  def apply(in: String, validation: List[List[Rule[A]]]): Future[A]
}

class CliInterpreter[
  SupportedTell <: HList : TypeclassList[?, TellAsterisk],
  SupportedAsk  <: HList : TypeclassList[?, AskAsterisk]
] extends Language[Future, SupportedTell, SupportedAsk] {

  override def interact[Tell, Ask](
    id            : String,
    t             : Tell,
    default       : Option[Ask],
    validation    : List[List[Rule[Ask]]],
    customContent : Map[String,(String,List[Any])]
  )( implicit
    selectorTell : IndexOf[SupportedTell, Tell],
    selectorAsk  : IndexOf[SupportedAsk, Ask]
  ): Future[Ask] = {
    // val teller = the[TypeclassList[SupportedTell,TellAsterisk]].
    //   forType[Tell].say(id, t)
    val asker = the[TypeclassList[SupportedAsk,AskAsterisk]].
      forType[Ask]
    asker(id, validation)
  }
}
