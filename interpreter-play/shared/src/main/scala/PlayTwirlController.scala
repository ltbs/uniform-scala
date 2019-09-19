package ltbs.uniform
package interpreters.playframework

import play.twirl.api.Html
import play.api._,mvc._
import concurrent.ExecutionContext
import cats.Monoid

abstract class PlayTwirlInterpreter(controller: Results)(
  implicit ec: ExecutionContext,
  mon: Monoid[Html]
) extends PlayInterpreter[Html](controller) {

}
