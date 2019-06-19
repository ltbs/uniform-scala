package ltbs.uniform
package interpreters.logictable

import shapeless._, ops.hlist.Selector
import reflect.runtime.universe.WeakTypeTag
import cats.data._

case class LogicTableInterpreter[
  SupportedTell <: HList,
  SupportedAsk  <: HList
]()(implicit
  tellSummoner : Summoner[SupportedTell, TellRenderer],
  askSummoner  : Summoner[SupportedAsk, SampleData]
) extends Language[Logic, SupportedTell, SupportedAsk] {

  override def interact[Tell: WeakTypeTag, Ask: WeakTypeTag](
    id: String,
    t: Tell,
    default: Option[Ask],
    validation: List[List[Rule[Ask]]],
    customContent: Map[String,(String,List[Any])]
  )(
    implicit selectorTell : Selector[SupportedTell, Tell],
    selectorAsk : Selector[SupportedAsk, Ask]
  ): Logic[Ask] = {
    val tellStrings = tellSummoner.instanceFor[Tell].apply(id, t).map {
      s"$id (tell): " ++ _
    }
    val askSamples = askSummoner.instanceFor[Ask].apply(id)

    EitherT {
      WriterT {
        askSamples.map { sample =>
          (
            tellStrings :+ s"$id ask: ${sample.toString}",
            validation.combined.either(sample)
          )
        }
      }
    }
  }
}
