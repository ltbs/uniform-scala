package ltbs.uniform.prototype

import cats.Eval
import cats.data._
import org.atnos.eff._
import cats.implicits._
import org.atnos.eff.all._
import org.atnos.eff.syntax.all._
import org.querki.jquery._
import ltbs.uniform._
import UniformTest._
import cats.data.{ Writer, State, ValidatedNel }

object JsInterpreter {

  case class Page(title: Option[String] = None,
                  body: Option[String] = None,
                  errors: List[ValidationError] = Nil)

  type Encoded = String
  type DB = Map[String,Encoded]
//  type Page = String
  type ValidationError = String
  type ValidatedData[A] = Option[ValidatedNel[ValidationError, A]]

  def validationFix[X,A](v: Validated[X, A]): ValidatedNel[X,A] =
    v.bimap(NonEmptyList(_,Nil), identity)

  trait Form[T] {
    def render(existing: ValidatedData[T]): String
    def fromNode(fieldSet: JQuery): ValidatedNel[ValidationError,T]
    def encode(in: T): Encoded
    def decode(out: Encoded): T
  }

  type JsStack = Fx.fx4[Reader[String, ?], Eval, State[DB, ?], Either[Page, ?]]

  implicit class JsEffectOps[R, A](e: Eff[R, A]) {
    type _state[Q]  = State[DB,?] |= Q
    type _either[Q] = Either[Page,?] |= Q
    type _readStage[Q] = Reader[String,?] |= Q

    def useForm[C, U](
      form: Form[C]
    )(
      implicit member: Member.Aux[UniformAsk[C,?], R, U],
      evalM: _eval[U],
      stateM: _state[U],
      eitherM: _either[U],
      readStage: _readStage[U]
    ): Eff[U, A] = e.translate(
      new Translate[UniformAsk[C,?], U] {
        def apply[X](ax: UniformAsk[C,X]): Eff[U, X] = {
          ax match {
            case UniformAsk(key, validation) =>
              val i: Eff[U,X] = for {
                page <- ask[U, String]
                state <- get[U, DB]
                va <- {
                  if (page != key) {
                    state.get(key).map{d => validation(form.decode(d).asInstanceOf[X])} match {
                      case None =>
                        left[U, Page, X](Page(title = key.some, body = form.render(None).some))
                      case Some(Validated.Valid(x)) =>
                        right[U, Page, X](x)
                      case Some(bad@Validated.Invalid(e)) =>
                        left[U, Page, X](Page(title = key.some, body = form.render(None).some, errors = List(e)))
                    }
                  } else {
                    val data: ValidatedNel[ValidationError, X] =
                      form.fromNode($("#content")) map {_.asInstanceOf[X]} andThen {x => validationFix(validation(x))}
                    println(data)
                    data match {
                      case Validated.Invalid(nel) =>
                        left[U, Page, X](Page(errors = nel.toList))
                      case Validated.Valid(x) =>
                        put[U,DB](state + (key -> form.encode(x.asInstanceOf[C]))) >> right[U, Page, X](x)
                    }
                  }
                }
              } yield va
              i
          }
        }
      }
    )
  }
}
