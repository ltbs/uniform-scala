package ltbs.uniform

import scala.language.higherKinds

import cats.implicits._
import ltbs.uniform.{Uniform => U}
import izumi.reflect.macrortti.LightTypeTag
import izumi.reflect.Tag
import validation.Rule
import com.github.ghik.silencer.silent

trait MonadInterpreter[F[+_], INTERACTTC[_,_], ASKLISTTC[_]] extends Interpreter[F, INTERACTTC, ASKLISTTC]{

  implicit def monadInstance: cats.Monad[F]

  protected def interactImpl[T,A](
    key: String,
    tellValue: T,
    default: Option[A],
    validation: Rule[A],
    customContent: Map[String,(String,List[Any])],
    interaction: INTERACTTC[T,A]
  ): F[A]

  @silent("never used")
  protected def subjourneyImpl[A](
    path: List[String],
    inner: F[A]
  ): F[A] = inner

  protected def askListImpl[A](
    key: String,
    askJourney: (Option[Int], List[A]) => F[A],
    default: Option[List[A]],
    validation: Rule[List[A]],
    customContent: Map[String,(String,List[Any])],    
    asker: ASKLISTTC[A]
  ): F[List[A]]

  @silent("erasure") override def interpretImpl[H <: Needs[_,_], T: Tag, A: Tag, E[_]](
    program: Uniform[H, T, A], 
    interactMap: Map[(LightTypeTag, LightTypeTag), INTERACTTC[_,_]],
    convertMap: Map[(LightTypeTag, LightTypeTag), Any],
    listAskMap: Map[LightTypeTag, ASKLISTTC[_]] 
  ): F[A] = {
    program match {
      case U.Map(base, f) =>
        interpretImpl(base, interactMap, convertMap, listAskMap ).map(f)
      case U.FlatMap(base, f) =>
        val g = f.map(interpretImpl(_, interactMap, convertMap, listAskMap))
        interpretImpl(base, interactMap, convertMap, listAskMap).flatMap(g)
      case U.Interact(key, value, default, validation, customContent, tellTag: Tag[T], askTag) =>
        interactImpl[T, A](
          key,
          value,
          default,
          validation,
          customContent,
          interactMap((tellTag.tag, askTag.tag)).asInstanceOf[INTERACTTC[T, A]]
        )
      case U.Pure(v) =>
        v.pure[F]
      case U.Subjourney(path, inner) =>
        subjourneyImpl(path, interpretImpl(inner, interactMap, convertMap, listAskMap))
      case U.Convert(key, action, tagF, tagA) =>
        convertImpl[E, A](
          key, 
          action.asInstanceOf[() => E[A]],
          convertMap((tagF.tag, tagA.tag)).asInstanceOf[Converter[E, F, A]]
        )
      case U.ListOf(key, base, default, validation, customContent, tag: Tag[A]) => 
        askListImpl[A](
          key,
          (index, existing) => interpretImpl(base(index, existing), interactMap, convertMap, listAskMap),
          default,
          validation,
          customContent, 
          listAskMap(tag.tag).asInstanceOf[ASKLISTTC[A]]
        )
    }
  }
}
