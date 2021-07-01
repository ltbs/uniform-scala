package ltbs.uniform

import scala.language.higherKinds
import scala.language.experimental.macros
import izumi.reflect.macrortti.LightTypeTag
import izumi.reflect.Tag
import com.github.ghik.silencer.silent

trait Interpreter[F[_], INTERACTTC[_,_], ASKLISTTC[_]] {

  protected def convertImpl[E[_], A](key: String, in: () => E[A], transformation: Converter[E, F, A]): F[A] = 
    transformation(key, in)

  def interpretImpl[H <: Needs[_,_], T: Tag, A: Tag, E[_]](
    program: Uniform[H, T, A], 
    interactMap: Map[(LightTypeTag, LightTypeTag), INTERACTTC[_,_]],    
    convertMap: Map[(LightTypeTag, LightTypeTag), Any], // effectively Map[(TagE, TagA), Converter[E,A,_]]. I hope to remove this field and have it handled directly in the macros
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

  @silent("dead code")
  def interpret[H <: Needs[_,_],T, A](
    program: Uniform[H, T, A]
  ): F[A] = macro InterpreterMacros.interpreter_impl[H, A, INTERACTTC, ASKLISTTC, F, T]
}
