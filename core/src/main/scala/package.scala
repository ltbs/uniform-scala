package ltbs

import scala.language.higherKinds

import cats.implicits._
import cats.{Monoid, Semigroup}
import cats.data.{NonEmptyList, Validated}
import collection.immutable.ListMap
import uniform.validation.{Rule, Transformation}
import izumi.reflect.{Tag, TagK}

package object uniform
    extends TreeLike.ToTreeLikeOps
    with TreeLikeInstances
    with ScalaVersionCompatibility
{

  type InputPath = List[String]
  type Input = Map[InputPath, List[String]]
  type ErrorTree = ListMap[NonEmptyList[InputPath], NonEmptyList[ErrorMsg]]

  implicit object Input extends MapTree[String, List[String]] {

    /** Extract an [[Input]] from a UTF-8 URL encoded String. */
    def fromUrlEncodedString(in: String): Either[ErrorTree,Input] = {
      val ungrouped: List[(String, String)] =
        in.split("&").toList
          .map{_.split("=").toList}
          .collect { case (k::v::Nil) => k ->
            java.net.URLDecoder.decode(v, "UTF-8")
          }

      ungrouped.groupBy(_._1).map{ case (k, comb) =>
        k.split("[.]").toList.dropWhile(_.isEmpty) -> comb.map {_._2}
      }.asRight
    }
  }

  implicit class RichInput(input: Input) {

    /** Generate a UTF-8 URL encoded String. */    
    def toUrlEncodedString: String = {
      input
        .flatMap { case (k, vs) =>
          vs.map { v =>
            s"""${k.mkString(".")}=${java.net.URLEncoder.encode(v, "UTF-8")}"""
          }
        }
        .mkString("&")
    }

    /** Take the string at the root of the input tree and pass it
      * through the transformation pipeline provided. This is usually
      * done when constructing a codec for a datatype. 
      * 
      * For example - 
      * {{{
      * val someBool: Validated[ErrorTree, Boolean] = 
      *   someInput.toField[Boolean]{x: String =>
      *     Validated.catchOnly[IllegalArgumentException](
      *       x.toBoolean
      *     ).leftMap(_ => ErrorMsg("invalid").toTree)
      *   }
      * }}}
      * */
    def toField[A](
      pipeline: Transformation[String, A]
    ): Validated[ErrorTree, A] =
      pipeline(
        input.valueAtRoot.flatMap(_.headOption.map(_.trim)).getOrElse("")
      )

    /** Extract a string from the root of the Input tree. A more
      * specialised version of [[toField]] that only handles
      * strings. */
    def toStringField(
      pipeline: Rule[String] = {Validated.Valid(_)}
    ): Validated[ErrorTree, String] = toField[String](pipeline)

    /** Extract a string from a child element, then transform it into the desired datatype */
    def subField[A](
      key: String,
      pipeline: Transformation[String, A]
    ): Validated[ErrorTree, A] =
      pipeline(
        input.valueAt(key).flatMap(_.headOption.map(_.trim)).getOrElse("")
      ).leftMap(_.prefixWith(key))

    /** Extract a string from a child element */    
    def stringSubField(
      key: String,
      pipeline: Rule[String] = {Validated.Valid(_)}
    ): Validated[ErrorTree, String] = subField(key, pipeline)


  }

  implicit class RichErrorTree(a: ErrorTree) {

    def valueAtRootList: List[ErrorMsg] =
      a.valueAtRoot.fold(List.empty[ErrorMsg])(_.toList)

    def errorsAtRoot = valueAtRootList

    def errorsAt(key: String): List[ErrorMsg] =
      a.valueAt(key).fold(List.empty[ErrorMsg])(_.toList)

    def errorsAtPath(key: List[String]): List[ErrorMsg] =
      a.valueAtPath(key).fold(List.empty[ErrorMsg])(_.toList)

  }

  implicit def monListMap[K,V: Semigroup] = new Monoid[ListMap[K,V]] {
    def empty = ListMap.empty

    def combine(xs: ListMap[K, V], ys: ListMap[K, V]): ListMap[K, V] =
      ys.foldLeft(xs) {
        case (mx, (k, y)) =>
          mx.updated(k, Semigroup.maybeCombine(mx.get(k), y))
      }
  }

  def interact[A] = new InteractBuilder[A]

  def ask[A: Tag](
    key: String,
    default: Option[A] = None,
    validation: Rule[A] = Rule.alwaysPass[A],
    customContent: Map[String,(String,List[Any])] = Map.empty
  ): Uniform[Needs.Ask[A], A, Unit] = Uniform.Ask(key, default, validation, customContent, implicitly[Tag[A]])

  def tell[A: Tag](
    key: String,
    value: A,
    customContent: Map[String,(String,List[Any])] = Map.empty    
  ): Uniform[Needs.Tell[A], Unit, A] =
    Uniform.Tell(
      key,
      value,
      customContent,
      implicitly[Tag[A]]
    )

  def end[A: Tag](
    key: String,
    value: A,
    customContent: Map[String,(String,List[Any])]
  ): Uniform[Needs.Tell[A], Nothing, A] =
    Uniform.EndTell(
      key,
      value,
      customContent,
      implicitly[Tag[A]]
    )

  def end(
    key: String,
    customContent: Map[String,(String,List[Any])]
  ): Uniform[Needs[Any], Nothing, Unit] = Uniform.End(
    key,
    customContent
  )

  def end[A: Tag](
    key: String,
    value: A
  ): Uniform[Needs.Tell[A], Nothing, A] =
    Uniform.EndTell(
      key,
      value,
      Map.empty,
      implicitly[Tag[A]]
    )

  def end(
    key: String
  ): Uniform[Needs[Any], Nothing, Unit] = Uniform.End(
    key,
    Map.empty
  )

  def endIf[T: Tag](
    pred: Boolean,
    key: String,
    value: T,
    customContent: Map[String,(String,List[Any])]
  ): Uniform[Needs.Tell[T], Unit, T] =
    if(pred) end(key, value, customContent) else Uniform.Pure(())

  def endIf[T: Tag](
    pred: Boolean,
    key: String,
    value: T
  ): Uniform[Needs.Tell[T], Unit, T] =
    if(pred) end(key, value, Map.empty) else Uniform.Pure(())

  def endIf(
    pred: Boolean,
    key: String,
    customContent: Map[String,(String,List[Any])]
  ): Uniform[Needs[Any], Unit, Unit] =
    if(pred) end(key, customContent) else Uniform.Pure(())

  def endIf(
    pred: Boolean,
    key: String
  ): Uniform[Needs[Any], Unit, Unit] =
    if(pred) end(key, Map.empty) else Uniform.Pure(())

  def pure[A](value: A) = Uniform.Pure(value)

  def subJourney[R <: Needs[_], A, T](
    pathHead: String,
    pathTail: String*
  )(base: Uniform[R, A, T]): Uniform[R, A, T] =
    Uniform.Subjourney[R,A,T](pathHead :: pathTail.toList, base)

  def convert[F[_], A](action: F[A])(implicit tag: TagK[F]): Uniform[Needs.Convert[F[_]], A, Unit] =
    Uniform.Convert(action, tag)

  def askList[A](
    key: String,
    default: Option[List[A]] = None, 
    validation: Rule[List[A]] = Rule.alwaysPass[List[A]]
  ) = new AskListBuilder[A](key, default, validation)

}

