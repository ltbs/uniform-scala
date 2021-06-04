package ltbs.uniform

import scala.language.higherKinds

import cats.implicits._
import cats.~>
import ltbs.uniform.{Uniform => U}
import izumi.reflect.macrortti.LightTypeTag
import izumi.reflect.Tag
import validation.Rule
import com.github.ghik.silencer.silent

trait MonadInterpreter[F[+_], TELLTC[_], ASKTC[_], ASKLISTTC[_]] extends Interpreter[F, TELLTC, ASKTC, ASKLISTTC]{

  implicit def monadInstance: cats.Monad[F]

  protected def askImpl[A](
    key: String,
    default: Option[A],
    validation: Rule[A],
    customContent: Map[String,(String,List[Any])],
    asker: ASKTC[A]
  ): F[A]

  protected def tellImpl[T](
    key: String,
    value: T,
    customContent: Map[String,(String,List[Any])],
    teller: TELLTC[T]
  ): F[Unit]

  protected def interactImpl[A,T](
    key: String,
    tellValue: T,
    default: Option[A],
    validation: Rule[A],
    customContent: Map[String,(String,List[Any])],
    asker: ASKTC[A],
    teller: TELLTC[T]
  ): F[A] = tellImpl(key, tellValue, customContent, teller) >>
    askImpl(key, default, validation, customContent, asker)

  protected def endTellImpl[T](
    key: String,
    value: T,
    customContent: Map[String,(String,List[Any])],
    teller: TELLTC[T]
  ): F[Nothing] =
    tellImpl(key, value, customContent, teller) >> endImpl(key, customContent)

  protected def endImpl(
    key: String,
    customContent: Map[String,(String,List[Any])]
  ): F[Nothing]

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

  protected def convertImpl[E[_], A](in: E[A], transformation: E ~> F): F[A] =
    transformation(in)

  @silent("erasure") override def interpretImpl[H <: Needs[_], T: Tag, A: Tag, E[_]](
    program: Uniform[H, T, A], 
    askMap: Map[LightTypeTag, ASKTC[_]],    
    tellMap: Map[LightTypeTag, TELLTC[_]],
    convertMap: Map[LightTypeTag, Any],
    listAskMap: Map[LightTypeTag, ASKLISTTC[_]] 
  ): F[A] = {
    program match {
      case U.Map(base, f) =>
        interpretImpl(base, askMap, tellMap, convertMap, listAskMap ).map(f)
      case U.FlatMap(base, f) =>
        val g = f.map(interpretImpl(_, askMap, tellMap, convertMap, listAskMap))
        interpretImpl(base, askMap, tellMap, convertMap, listAskMap).flatMap(g)
      case U.Tell(key, value, customContent, tag: Tag[T]) => tellImpl[T](
          key,
          value,
          customContent,
          tellMap(tag.tag).asInstanceOf[TELLTC[T]]
        )
      case U.Interact(key, value, default, validation, customContent, askTag, tellTag: Tag[T]) =>
        interactImpl[A, T](
          key,
          value,
          default,
          validation,
          customContent,
          askMap(askTag.tag).asInstanceOf[ASKTC[A]],
          tellMap(tellTag.tag).asInstanceOf[TELLTC[T]]
        )
      case U.Ask(key, default, validation, customContent, tag) =>
        askImpl(
          key,
          default,
          validation,
          customContent,
          askMap(tag.tag).asInstanceOf[ASKTC[A]]
        )
      case U.EndTell(key, value, customContent, tag: Tag[T]) =>
        endTellImpl[T](
          key,
          value,
          customContent,
          tellMap(tag.tag).asInstanceOf[TELLTC[T]]
        )
      case U.End(key, customContent) =>
        endImpl(key, customContent)
      case U.Pure(v) =>
        v.pure[F]
      case U.Subjourney(path, inner) =>
        subjourneyImpl(path, interpretImpl(inner, askMap, tellMap, convertMap, listAskMap))
      case U.Convert(action, tag) =>
        convertImpl[E, A](
          action.asInstanceOf[E[A]],
          convertMap(tag.tag).asInstanceOf[E ~> F]
        )
      case U.ListOf(key, base, default, validation, customContent, tag: Tag[A]) => 
        askListImpl[A](
          key,
          (index, existing) => interpretImpl(base(index, existing), askMap, tellMap, convertMap, listAskMap),
          default,
          validation,
          customContent, 
          listAskMap(tag.tag).asInstanceOf[ASKLISTTC[A]]
        )
    }
  }
}
