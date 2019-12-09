package ltbs.uniform
package interpreters.logictable

import shapeless._
import cats.data._
import validation._

case class LogicTableInterpreter[
  SupportedTell <: HList,
  SupportedAsk  <: HList
]()(implicit
  tellSummoner : TypeclassList[SupportedTell, TellRenderer],
  askSummoner  : TypeclassList[SupportedAsk, SampleData]
) extends Language[Logic, SupportedTell, SupportedAsk] {

  override def interact[Tell, Ask](
    id            : String,
    t             : Tell,
    default       : Option[Ask],
    validation    : Rule[Ask],
    customContent : Map[String,(String,List[Any])]
  )( implicit
    selectorTell : IndexOf[SupportedTell, Tell],
    selectorAsk  : IndexOf[SupportedAsk, Ask]
  ): Logic[Ask] = {
    val tellStrings = tellSummoner.forType[Tell].apply(id, t).map {
      s"$id (tell): " ++ _
    }
    val askSamples = askSummoner.forType[Ask].apply(id)

    EitherT {
      WriterT {
        askSamples.map { sample =>
          (
            tellStrings :+ s"$id ask: ${sample.toString}",
            validation.either(sample)
          )
        }
      }
    }
  }

}
