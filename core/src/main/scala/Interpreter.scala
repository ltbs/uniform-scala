package ltbs.uniform

import scala.language.higherKinds
import cats.~>
import scala.language.experimental.macros
import izumi.reflect.macrortti.LightTypeTag
import izumi.reflect.Tag

trait Interpreter[F[_], ASKTC[_], TELLTC[_]] {

  def executeImpl[H <: Needs[_], A: Tag](
    program: Uniform[H, A], 
    askMap: Map[LightTypeTag, ASKTC[_]],    
    tellMap: Map[LightTypeTag, TELLTC[_]],
  ): F[A]

  def transform[G[_]](f: F ~> G) = {
    val that = this
    new Interpreter[G, ASKTC, TELLTC] {
      def executeImpl[H <: Needs[_], A: Tag](
        program: Uniform[H, A],
        askMap: Map[LightTypeTag, ASKTC[_]],
        tellMap: Map[LightTypeTag, TELLTC[_]]
      ): G[A] = f(that.executeImpl[H, A](program, askMap, tellMap))
    }
  }

  def execute[H <: Needs[_],A](
    program: Uniform[H, A]
  ): F[A] = macro TypeclassListMacros.interpreter_impl[H, A, ASKTC, TELLTC, F]

}

