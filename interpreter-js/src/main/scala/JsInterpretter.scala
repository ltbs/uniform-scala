package ltbs.uniform.prototype

import cats.Eval
import cats.data._
import org.atnos.eff._
import cats.implicits._
import org.atnos.eff.all._
import org.atnos.eff.syntax.all._
import org.querki.jquery._
import ltbs.uniform._
import cats.data.State
import ltbs.uniform.datapipeline.Tree

object JsInterpreter {

  case class Page(title: Option[String] = None,
                  body: Option[String] = None,
                  errors: ErrorTree = Tree.empty)

  type Encoded = String
  type DB = Map[String,Encoded]
  //  type Page = String
  type ErrorTree = Tree[String,String]

  trait Form[T] {
    def render(key: String, existing: Option[T]): String
    def fromNode(key: String, fieldSet: JQuery): Either[ErrorTree, T]
    def encode(in: T): Encoded
    def decode(out: Encoded): Either[ErrorTree,T]
  }

  type JsStack = Fx.fx4[Reader[String, ?], Eval, State[DB, ?], Either[Page, ?]]

  implicit class JsEffectOps[R, A](e: Eff[R, A]) {
    type _state[Q]  = State[DB,?] |= Q
    type _either[Q] = Either[Page,?] |= Q
    type _readStage[Q] = Reader[String,?] |= Q

    def validationToErrorTree[V](f: V => Validated[String,V]): V => Either[ErrorTree,V] = {
      x => f(x).toEither.leftMap(Tree(_))
    }      

    def useForm[C, U](
      form: Form[C]
    )(
      implicit member: Member.Aux[UniformAsk[C,?], R, U],
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
                    state.get(key).map{d => form.decode(d).map(_.asInstanceOf[X]).flatMap{
                      validationToErrorTree(validation)
                    }} match {
                      case None =>
                        left[U, Page, X](Page(title = key.some, body = form.render(key, None).some))
                      case Some(Right(x)) =>
                        right[U, Page, X](x)
                      case Some(Left(e)) =>
                        left[U, Page, X](Page(title = key.some, body = form.render(key, None).some, e))
                    }
                  } else {
                    val data =
                      form.fromNode(key, $("#content")) map {_.asInstanceOf[X]} 
                    println(data)
                    data >>= validationToErrorTree(validation) match {
                      case Left(e) =>
                        left[U, Page, X](Page(errors = e))
                      case Right(x) =>
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
