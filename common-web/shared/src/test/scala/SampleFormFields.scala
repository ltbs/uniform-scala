package ltbs.uniform
package common.web

import cats.implicits._
import com.github.ghik.silencer.silent

object SampleFormFields extends SampleFormFields

trait SampleFormFields {

  implicit val twirlUnitField = new FormField[String,Unit] {
    def decode(out: Input): Either[ltbs.uniform.ErrorTree,Unit] = Right(())
    def encode(in: Unit): Input = Input.empty
    def render(
      @silent("never used") pageKey: List[String],
      fieldKey: List[String],      
      path: Breadcrumbs,
      data: Input,
      errors: ErrorTree,
      messages: UniformMessages[String]
    ): Option[String] = Some("")
  }


  implicit val stringFieldR = new FormField[String, String] {
    def render(
      @silent("never used") pageKey: List[String],
      fieldKey: List[String],      
      @silent("never used") path: Breadcrumbs,
      @silent("never used") data: Input,
      @silent("never used") errors: ErrorTree,
      @silent("never used") messages: UniformMessages[String]
    ): Option[String] = Some {
      val k = fieldKey.mkString(".")
      s"STRING[$k]"
    }

    def decode(out: Input): Either[ErrorTree,String] = {
      val root: Option[String] = out.valueAtRoot
        .flatMap(_.filter(_.trim.nonEmpty).headOption)

      root match {
        case None => Left(ErrorMsg("required").toTree)
        case Some(data) => Right(data)
      }
    }

    def encode(in: String): Input = Input.one(List(in))

  }

  implicit val intFieldR = new FormField[String, Int] {
    override def codec: Codec[Int] = stringFieldR.
      simap(x =>
        Either.catchOnly[NumberFormatException](x.toInt)
          .leftMap(_ => ErrorMsg("bad.value").toTree)
      )(_.toString)

    def render(
      @silent("never used") pageKey: List[String],     
      fieldKey: List[String],
      @silent("never used") path: Breadcrumbs,
      @silent("never used") data: Input,
      @silent("never used") errors: ErrorTree,
      @silent("never used") messages: UniformMessages[String]
    ): Option[String] = Some {
      val k = fieldKey.mkString(".")
      s"INT[$k]"
    }

    def decode(out: Input): Either[ErrorTree, Int] =
      codec.decode(out)

    def encode(in: Int): Input = codec.encode(in)
  }

}
