package ltbs.uniform.web

import shapeless._
import shapeless.labelled._
import cats.implicits._ // needed for monadic either in 2.11
import ltbs.uniform.{Tree,ErrorTree}

object InferParser {

  implicit val parserHNil: DataParser[HNil] = new DataParser[HNil] {
    def bind(in: Input): Either[ErrorTree,HNil] = Right(HNil)
    def unbind(a: HNil): Input = Tree(Nil)
  }

  implicit def hConsParser[K <: Symbol, H, T <: HList](
    implicit
      witness: Witness.Aux[K],
    hParser: Lazy[DataParser[H]],
    tParser: DataParser[T]
  ): DataParser[FieldType[K,H] :: T] = {
    val fieldName: String = witness.value.name

    new DataParser[FieldType[K,H] :: T] {

      def bind(in: Input): Either[ErrorTree,FieldType[K,H] :: T] = {
        val tailV = tParser.bind(in)
        val headV = in.get(fieldName) flatMap hParser.value.bind

        (headV,tailV) match {
          case (Right(h), Right(t)) => Right((field[K](h) :: t))
          case (Left(he), Left(te)) => Left(te.add(fieldName, he))
          case (_,        Left(te)) => Left(te)
          case (Left(he), _)        => Left(Tree("",Map(fieldName -> he)))
        }
      }

      def unbind(a: FieldType[K,H] :: T): Input = {
        val tailData: Input = tParser.unbind(a.tail)
        val headData: Input = hParser.value.unbind(a.head)
        tailData.add(fieldName, headData)
      }
    }
  }

  implicit val cnilParser: DataParser[CNil] =
    new DataParser[CNil] {
      def bind(in: Input): Either[ErrorTree,CNil] =
        Left(Tree("required"))
      def unbind(a: CNil): Input = Tree.empty
    }

  implicit def coproductParser[K <: Symbol, H, T <: Coproduct](
    implicit
      witness: Witness.Aux[K],
    hparser: DataParser[H],
    tparser: DataParser[T]
  ): DataParser[FieldType[K, H] :+: T] =
    new DataParser[FieldType[K, H] :+: T] {

      val fname = witness.value.name

      def bind(in: Input): Either[ErrorTree,FieldType[K, H] :+: T] = {
        if (in.value.headOption == Some(fname)) {
          hparser.bind(
            in.forestAtPath(fname).getOrElse(Tree.empty)
          ).map(x => Inl(field[K](x)))
        } else {
          tparser.bind(in).map(x => Inr(x))
        }
      }

      def unbind(a: FieldType[K, H] :+: T): Input = a match {
        case Inl(l) =>

          Tree(List(fname), Map(fname -> hparser.unbind(l)))
        case Inr(r) => tparser.unbind(r)
      }
    }


  implicit def genericParser[A, H, T]
    (implicit
       generic: LabelledGeneric.Aux[A,T],
     hGenParser: Lazy[DataParser[T]]
    ): DataParser[A] = new DataParser[A]
  {

    def bind(in: Input): Either[ErrorTree,A] =
      hGenParser.value.bind(in).map(generic.from)

    def unbind(a:A): Input =
      hGenParser.value.unbind(generic.to(a))
  }

}
