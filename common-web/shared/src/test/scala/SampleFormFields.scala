package ltbs.uniform
package common.web

import cats.implicits._
import com.github.ghik.silencer.silent

object SampleFormFields extends SampleFormFields

trait SampleFormFields {

  implicit val twirlUnitField = new FormField[Unit,String] {
    def decode(out: Input): Either[ltbs.uniform.ErrorTree,Unit] = Right(())
    def encode(in: Unit): Input = Input.empty
    def render(
      key: List[String],
      path: Breadcrumbs,
      data: Input,
      errors: ErrorTree,
      messages: UniformMessages[String]
    ): String = ""
  }


  implicit val stringFieldR = new FormField[String, String] {
    def render(
      key: List[String],
      @silent("never used") path: Breadcrumbs,
      @silent("never used") data: Input,
      @silent("never used") errors: ErrorTree,
      @silent("never used") messages: UniformMessages[String]
    ): String = {
      val k = key.mkString(".")
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

  implicit val intFieldR = new FormField[Int, String] {
    val codec: Codec[Int] = stringFieldR.
      simap(x =>
        Either.catchOnly[NumberFormatException](x.toInt)
          .leftMap(_ => ErrorMsg("bad.value").toTree)
      )(_.toString)

    def render(
      key: List[String],
      @silent("never used") path: Breadcrumbs,
      @silent("never used") data: Input,
      @silent("never used") errors: ErrorTree,
      @silent("never used") messages: UniformMessages[String]
    ): String = {
      val k = key.mkString(".")
      s"INT[$k]"
    }

    def decode(out: Input): Either[ErrorTree, Int] =
      codec.decode(out)

    def encode(in: Int): Input = codec.encode(in)
  }

}
