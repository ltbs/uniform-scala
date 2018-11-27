package ltbs.uniform.datapipeline

import shapeless._
import shapeless.labelled._

object InferParser {

  implicit val parserHNil: DataParser[HNil] = new DataParser[HNil] {
    def bind(in: Input): Either[Error,HNil] = Right(HNil)
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

      // this should probably be done applicative
      def bindOld(in: Input): Either[Error,FieldType[K,H] :: T] = for {
        t <- tParser.bind(in)
        hdata <- in.get(fieldName)
        h <- hParser.value.bind(hdata)
      } yield (field[K](h) :: t)

      def bind(in: Input): Either[Error,FieldType[K,H] :: T] = {
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

  implicit def genericParser[A, H, T]
    (implicit
       generic: LabelledGeneric.Aux[A,T],
     hGenParser: Lazy[DataParser[T]]
    ): DataParser[A] = new DataParser[A]
  {

    def bind(in: Input): Either[Error,A] =
      hGenParser.value.bind(in).map(generic.from)

    def unbind(a:A): Input =
      hGenParser.value.unbind(generic.to(a))
  }

}
