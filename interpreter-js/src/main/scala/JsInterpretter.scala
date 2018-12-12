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

  sealed trait Action {
    def key: String
  }
  case class Back(key: String) extends Action
  case class Submit(key: String) extends Action

  def extractData(fieldSet: JQuery): Tree[String,List[String]] = {
    val fields = $("fieldset.uniform").serialize()
    val decoded=dpl.decodeUrlString(fields)
    dpl.formToInput(decoded)
  }

  case class Page(
    title: Option[String] = None,
    breadcrumbs: List[String],
    body: Option[String] = None,
    errors: ErrorTree = Tree.empty
  )

  type Encoded = String
  type DB = Map[String,Encoded]
  //  type Page = String
  type ErrorTree = Tree[String,String]

  trait Form[T] {
    def render(
      key: String,
      existing: Option[Tree[String,List[String]]],
      errors: ErrorTree
    ): String

    def fromNode(
      key: String,
      fieldSet: JQuery
    ): Either[ErrorTree, T]

    def encode(in: T): Encoded

    def decode(out: Encoded): Either[ErrorTree,T]

    def toDataTree(in: T): Tree[String,List[String]]
  }

  type JsStack = Fx.fx5[Reader[Action, ?], Eval, State[DB, ?], State[List[String],?] , Either[Page, ?]]

  implicit class JsEffectOps[R, A](e: Eff[R, A]) {
    type _state[Q]  = State[DB,?] |= Q
    type _either[Q] = Either[Page,?] |= Q
    type _readStage[Q] = Reader[Action,?] |= Q
    type _breadcrumbs[Q]  = State[List[String],?] |= Q

    def validationToErrorTree[V](f: V => Validated[String,V]): V => Either[ErrorTree,V] = {
      x => f(x).toEither.leftMap(Tree(_))
    }

    def useForm[C, U](
      form: Form[C]
    )(
      implicit member: Member.Aux[UniformAsk[C,?], R, U],
      stateM: _state[U],
      breadcrumbsM: _breadcrumbs[U],
      eitherM: _either[U],
      readStage: _readStage[U]
    ): Eff[U, A] = e.translate(
      new Translate[UniformAsk[C,?], U] {
        def apply[X](ax: UniformAsk[C,X]): Eff[U, X] = {
          ax match {
            case UniformAsk(key, validation) =>
              val i: Eff[U,X] = for {
                action <- ask[U, Action]
                state <- get[U, DB]
                breadcrumbs <- get[U, List[String]]
                va <- {

                  val dbData: Option[Either[ErrorTree,X]] =
                    state.get(key).map{d => form.decode(d).map(_.asInstanceOf[X]).flatMap{
                      validationToErrorTree(validation)
                    }}
                  println(s"dbdata: $dbData")
                  val formData: Either[ErrorTree,X] = form.fromNode(key, $("fieldset.uniform")) map {_.asInstanceOf[X]}


                      val dataTree: Tree[String, List[String]] = action match {
                        case Submit(_) =>
                          extractData($("#content"))
                        case Back(_) => state.get(key).map(dpl.decodeInput).getOrElse(Tree.empty[String, List[String]])
                      }

                  (action,dbData) match {
                    case (Submit(`key`),_) =>
                      formData >>= validationToErrorTree(validation) match {
                        case Left(e) =>
                          left[U, Page, X](Page(body = form.render(key, dataTree.some, e).some, errors = e, breadcrumbs = breadcrumbs))
                        case Right(x) =>
                          put[U, List[String]](key :: breadcrumbs) >>
                          put[U, DB](state + (key -> form.encode(x.asInstanceOf[C]))) >>
                          right[U, Page, X](x)
                      }
                    case (Submit(requestedPage),None) => left[U, Page, X](
                      Page(
                        title = key.some,
                        body = form.render(key, None, Tree.empty).some,
                        breadcrumbs=breadcrumbs
                      ))
                    case (Submit(requestedPage),Some(Right(x))) =>
                      put[U, List[String]](key :: breadcrumbs) >> right[U, Page, X](x)
                    case (Submit(requestedPage),Some(Left(e))) =>                      
                      left[U, Page, X](
                        Page(
                          title = key.some,
                          body = form.render(key, None, e).some,
                          errors=e,
                          breadcrumbs=breadcrumbs
                        ))
                    case (Back(`key`),_) | (Back(_),None) | (Back(_), Some(Left(_))) =>
                      val err = dbData match {
                        case Some(Left(e)) => e
                        case _ => Tree.empty[String, String]
                      }

                      val dataTree: Tree[String, List[String]] = dbData match {
                        case Some(Right(x)) => Tree(Nil, Map(key -> form.toDataTree(x.asInstanceOf[C])))
                        case _              => Tree.empty
                      }
                      println(s"dataTree: $dataTree")
                      left[U, Page, X](
                        Page(
                          title = key.some,
                          errors = err,
                          body = form.render(key, dataTree.some, err).some,
                          breadcrumbs=breadcrumbs
                        ))
                    case (Back(_),Some(Right(x))) =>                      
                      put[U, List[String]](key :: breadcrumbs) >> right[U, Page, X](x)
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
