package ltbs.uniform

import scala.language.higherKinds
import scala.language.experimental.macros
import izumi.reflect.macrortti.LightTypeTag
import izumi.reflect.Tag

trait Interpreter[F[_], ASKTC[_], TELLTC[_], ASKLISTTC[_]] {

  def interpretImpl[H <: Needs[_], A: Tag, T: Tag, E[_]](
    program: Uniform[H, A, T], 
    askMap: Map[LightTypeTag, ASKTC[_]],    
    tellMap: Map[LightTypeTag, TELLTC[_]],
    convertMap: Map[LightTypeTag, Any],
    listAskMap: Map[LightTypeTag, ASKLISTTC[_]]    
  ): F[A]

  // def transform[G[_]](f: F ~> G) = {
  //   val that = this
  //   new Interpreter[G, ASKTC, TELLTC] {
  //     def interpretImpl[H <: Needs[_], A: Tag, T: Tag, E[_]](
  //       program: Uniform[H, A, T],
  //       askMap: Map[LightTypeTag, ASKTC[_]],
  //       tellMap: Map[LightTypeTag, TELLTC[_]],
  //       convertMap: Map[LightTypeTag, ~>[E,F]]
  //     ): G[A] = f(that.interpretImpl[H, A, T, E](program, askMap, tellMap, convertMap))
  //   }
  // }

  def interpret[H <: Needs[_],A, T](
    program: Uniform[H, A, T]
  ): F[A] = macro InterpreterMacros.interpreter_impl[H, A, ASKTC, TELLTC, ASKLISTTC, F, T]
}
