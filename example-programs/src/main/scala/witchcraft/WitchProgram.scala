package ltbs.uniform
package examples

import scala.language.higherKinds

import validation._
import cats.Monad
import cats.implicits._

package object witchcraft {

  def witchProgram = for {
    f <- ask[List[Familiar]]("familiars", validation = Rule.lengthBetween(1, 3))
    e <- ask[List[Evidence]]("evidence", validation =
      Rule.forEachInList(Rule.cond({_ != Evidence.HasWartOnNose}, "dont-care-about-warts")))
    a <- ask[Accused]("accused")
  } yield WitchReport(a,e,f)

}
