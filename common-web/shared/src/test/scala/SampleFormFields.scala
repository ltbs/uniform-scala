package ltbs.uniform
package common.web

import cats.implicits._
import com.github.ghik.silencer.silent

object SampleFormFields extends SampleFormFields

trait SampleFormFields {

  implicit val twirlUnitField = new WebAsk[String,Unit] {
    def decode(out: Input): Either[ltbs.uniform.ErrorTree,Unit] = Right(())
    def encode(in: Unit): Input = Input.empty

    def render(
      pageIn: PageIn[String],
      stepDetails: StepDetails[String,Unit]
    ):Option[String] = Some("")

  }

  implicit val stringFieldR = new WebAsk[String, String] {

    def render(
      pageIn: PageIn[String],
      stepDetails: StepDetails[String,String]
    ):Option[String] = Some{
      val k = stepDetails.fieldKey.mkString(".")
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

  implicit val intFieldR = new WebAsk[String, Int] {
    override def codec: Codec[Int] = stringFieldR.
      simap(x =>
        Either.catchOnly[NumberFormatException](x.toInt)
          .leftMap(_ => ErrorMsg("bad.value").toTree)
      )(_.toString)

    def render(
      pageIn: PageIn[String],
      stepDetails: StepDetails[String,Int]
    ):Option[String] = Some{
      val k = stepDetails.fieldKey.mkString(".")
      s"Int[$k]"
    }

    def decode(out: Input): Either[ErrorTree, Int] =
      codec.decode(out)

    def encode(in: Int): Input = codec.encode(in)
  }

}
