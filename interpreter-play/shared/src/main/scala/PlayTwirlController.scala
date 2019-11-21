package ltbs.uniform
package interpreters.playframework

import play.twirl.api.Html
import play.api._,mvc._
import concurrent.ExecutionContext

abstract class PlayTwirlInterpreter(controller: Results)(
  implicit ec: ExecutionContext
) extends PlayInterpreter[Html](controller) {

}
