package ltbs.uniform
package interpreters.playframework

import ltbs.uniform.common.web.GenericWebTell
import play.twirl.api.Html
import play.api._,mvc._
import concurrent.ExecutionContext

abstract class PlayTwirlInterpreter(controller: Results)(
  implicit ec: ExecutionContext
) extends PlayInterpreter[Html](controller) {

  implicit def unitTell: GenericWebTell[Unit, Html] = new GenericWebTell[Unit, Html] {
    def render(in: Unit, key: String, messages: UniformMessages[Html]): Html = Html("")
  }

}
