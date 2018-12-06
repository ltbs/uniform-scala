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
import ltbs.uniform.{datapipeline => dpl}

object JsInterpreter {

  def extractData(fieldSet: JQuery): Tree[String,List[String]] = {
    val fields = $("fieldset.uniform").serialize()
    val decoded=dpl.decodeUrlString(fields)
    dpl.formToInput(decoded)
  }

  case class Page(title: Option[String] = None,
                  body: Option[String] = None,
                  errors: ErrorTree = Tree.empty)

  type Encoded = String
  type DB = Map[String,Encoded]
  //  type Page = String
  type ErrorTree = Tree[String,String]

  trait Form[T] {
    def render(key: String, existing: Option[Tree[String,List[String]]], errors: ErrorTree): String
    def fromNode(key: String, fieldSet: JQuery): Either[ErrorTree, T]

    def fromDataTree(key: String, datatree: Tree[String, List[String]]): Either[ErrorTree, T]    // is this needed anymore?

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
                        left[U, Page, X](Page(title = key.some, body = form.render(key, None, Tree.empty).some))
                      case Some(Right(x)) =>
                        right[U, Page, X](x)
                      case Some(Left(e)) =>
                        left[U, Page, X](Page(title = key.some, body = form.render(key, None, e).some, e))
                    }
                  } else {
                    val dataTree = extractData($("#content"))

                    val data = form.fromNode(key, $("fieldset.uniform")) map {_.asInstanceOf[X]}

                    println(s"DATA: $data")
                    data >>= validationToErrorTree(validation) match {
                      case Left(e) =>
                        left[U, Page, X](Page(body = form.render(key, dataTree.some, e).some, errors = e))
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
