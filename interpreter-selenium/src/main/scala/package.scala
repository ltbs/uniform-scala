package ltbs.uniform.interpreters

import cats.Eval
import cats.implicits._
import cats.data.Reader
import ltbs.uniform._
import org.atnos.eff._
import org.atnos.eff.all._
import org.atnos.eff.syntax.all._
import org.openqa.selenium.WebDriver

package object selenium {
  type SeleniumStack = Fx.fx2[Reader[WebDriver,?],Eval]

  type _readDriver[Q] = Reader[WebDriver,?] |= Q

  implicit class PlayEffectOps[R, A](e: Eff[R, A]) {

    def giveExample[C, U](
      example: C
    )(
      implicit
        inputMethod: SeleniumInputer[C],
      member: Member.Aux[UniformAsk[C,?], R, U],
      driverR: _readDriver[U],
      evalM: _eval[U]
    ): Eff[U, A] =
      giveExampleMap[C,U]{_ => example}(inputMethod, member, driverR, evalM)

    def giveExampleMap[C, U](
      exampleMap: Function[String,C]
    )(
      implicit
        inputMethod: SeleniumInputer[C],
      member: Member.Aux[UniformAsk[C,?], R, U],
      driverR: _readDriver[U],
      evalM:_eval[U]
    ): Eff[U, A] =
      e.translate(
        new Translate[UniformAsk[C,?], U] {
          def apply[X](ax: UniformAsk[C,X]): Eff[U, X] =
            ax match {
              case UniformAsk(key,validation) =>
                ask[U, WebDriver] >>= {driver => send{
                  val v = exampleMap(key)
                  inputMethod(v,driver) >> Eval.later{v.asInstanceOf[X]}
                }}
            }
        })

  }

}
