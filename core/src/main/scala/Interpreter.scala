package ltbs.uniform

import scala.language.higherKinds
import cats.~>
import scala.language.experimental.macros
import izumi.reflect.macrortti.LightTypeTag
import izumi.reflect.Tag

trait Interpreter[F[_], ASKTC[_], TELLTC[_]] {

  def interpretImpl[H <: Needs[_], A: Tag, T: Tag](
    program: Uniform[H, A, T], 
    askMap: Map[LightTypeTag, ASKTC[_]],    
    tellMap: Map[LightTypeTag, TELLTC[_]],
  ): F[A]

  def transform[G[_]](f: F ~> G) = {
    val that = this
    new Interpreter[G, ASKTC, TELLTC] {
      def interpretImpl[H <: Needs[_], A: Tag, T: Tag](
        program: Uniform[H, A, T],
        askMap: Map[LightTypeTag, ASKTC[_]],
        tellMap: Map[LightTypeTag, TELLTC[_]]
      ): G[A] = f(that.interpretImpl[H, A, T](program, askMap, tellMap))
    }
  }

  def interpret[H <: Needs[_],A, T](
    program: Uniform[H, A, T]
  ): F[A] = macro InterpreterMacros.interpreter_impl[H, A, ASKTC, TELLTC, F, T]
}
