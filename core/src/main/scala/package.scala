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

  implicit class RichInput(val input: Input) extends AnyVal {

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
  ): Uniform[Needs.Interact[Unit,A], Unit, A] = Uniform.Interact(key, (), default, validation, customContent, implicitly[Tag[Unit]], implicitly[Tag[A]])

  def tell[T: Tag](
    key: String,
    value: T,
    customContent: Map[String,(String,List[Any])] = Map.empty
  ): Uniform[Needs.Interact[T,Unit], T, Unit] =
    Uniform.Interact(
      key,
      value,
      None,
      Rule.alwaysPass[Unit],
      customContent,
      implicitly[Tag[T]],
      implicitly[Tag[Unit]]
    )

  def end[T: Tag](
    key: String,
    value: T,
    customContent: Map[String,(String,List[Any])]
  ): Uniform[Needs.Interact[T, Nothing], T, Nothing] =
    Uniform.End(
      key,
      value,
      customContent,
      implicitly[Tag[T]]
    )

  def end(
    key: String,
    customContent: Map[String,(String,List[Any])]
  ): Uniform[Needs.Interact[Unit, Nothing], Unit, Nothing] = Uniform.End(
    key,
    (),
    customContent,
    implicitly[Tag[Unit]]
  )

  def end[A: Tag](
    key: String,
    value: A
  ): Uniform[Needs.Interact[A,Nothing], A, Nothing] =
    Uniform.End(
      key,
      value,
      Map.empty,
      implicitly[Tag[A]]
    )

  def end(
    key: String
  ): Uniform[Needs.Interact[Unit, Nothing], Unit, Nothing] = Uniform.End(
    key,
    (),
    Map.empty,
    implicitly[Tag[Unit]]
  )

  def pure[A](value: A): Uniform[Needs[_,_], Any, A] = Uniform.Pure(value)

  def subJourney[R <: Needs[_,_], T, A](
    pathHead: String,
    pathTail: String*
  )(base: Uniform[R, T, A]): Uniform[R, T, A] =
    Uniform.Subjourney[R,T,A](pathHead :: pathTail.toList, base)

  def convertWithKey[F[_], A](key: String)(action: => F[A])(implicit tagF: TagK[F], tagA: Tag[A]): Uniform[Needs.Convert[F, A], Unit, A] =
    Uniform.Convert(key, () => action, tagF, tagA)

  def convert[F[_], A](action: => F[A])(implicit tagF: TagK[F], tagA: Tag[A]): Uniform[Needs.Convert[F, A], Unit, A] =
    convertWithKey(scala.util.Random.alphanumeric.take(20).mkString)(action)

  def askList[A](
    key: String,
    default: Option[List[A]] = None,
    validation: Rule[List[A]] = Rule.alwaysPass[List[A]]
  ) = new AskListBuilder[A](key, default, validation)

  def nonReturn(key: String, hard: Boolean = true) = Uniform.NonReturn(key, hard)

}
