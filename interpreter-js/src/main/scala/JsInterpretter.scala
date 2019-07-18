package ltbs.uniform
package interpreters.js

import ltbs.uniform._
import ltbs.uniform._
import shapeless.{Path ⇒ _, _}
import common.web._
import cats.implicits._
import cats.data._
import org.querki.jquery._
import concurrent._

abstract class JsInterpreter[Html](domSelector: String) {

  type DomMonad[A] = RWST[
    Future,
    (JourneyConfig, List[String], Option[Input]),
    Unit,
    (Path, DB),
    PageOut[A,Html]
  ]

  val dom = $(domSelector)

  type JsAsk[A]  = GenericWebAsk[A, Html]
  type JsTell[A] = GenericWebTell[A, Html]

  def messages(
    customContent: Map[String,(String,List[Any])]
  ): UniformMessages[Html]

  class FutureJSInterpreter[
    SupportedTell <: HList,
    SupportedAsk  <: HList
  ]( implicit
    tellSummoner : TypeclassList[SupportedTell, JsTell],
    askSummoner  : TypeclassList[SupportedAsk, JsAsk],
    ec           : concurrent.ExecutionContext
  ) extends Language[DomMonad, SupportedTell, SupportedAsk] {

    override def interact[Tell, Ask](
      id            : String,
      t             : Tell,
      default       : Option[Ask],
      validation    : List[List[Rule[Ask]]],
      customContent : Map[String,(String,List[Any])]
    )(
      implicit selectorTell : IndexOf[SupportedTell, Tell],
      selectorAsk : IndexOf[SupportedAsk, Ask]
    ): DomMonad[Ask] = {
      val asker = askSummoner.forType[Ask]
      val teller = tellSummoner.forType[Tell]
      RWST { case ((config, currentId, input), (path, db)) ⇒
        val localMessages = messages(customContent)
        val tellHtml = teller.render(t, id, localMessages)
        asker.page(
          targetId = id.split("/").toList.dropWhile(_.isEmpty),
          currentId,
          default,
          validation,
          config,
          input,
          path,
          db,
          localMessages
        ).map { ((), (path, db), _) }
      }
    }
  }

}
