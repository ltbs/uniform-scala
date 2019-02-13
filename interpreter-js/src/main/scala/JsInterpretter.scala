package ltbs.uniform.prototype

import cats.data._
import org.atnos.eff._
import cats.implicits._
import org.atnos.eff.all._
import org.atnos.eff.syntax.all._
import org.querki.jquery._
import ltbs.uniform._
import cats.data.State
import ltbs.uniform.{Tree}
import ltbs.uniform.web._
import play.twirl.api.Html

object JsInterpreter {

  sealed trait Action {
    def key: List[String]
  }
  case class Back(key: List[String]) extends Action
  case class Submit(key: List[String]) extends Action

  def extractData(fieldSet: JQuery): Tree[String,List[String]] = {
    val fields = $("fieldset.uniform").serialize()
    FormUrlEncoded.readString(fields).toInputTree
  }

  case class Page(
    title: Option[String] = None,
    breadcrumbs: List[List[String]],
    body: Option[String] = None,
    errors: ErrorTree = Tree.empty
  )

  type DB = Map[String,Encoded]

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

  type JsStack = Fx.fx2[State[(DB,List[String]), ?], Either[Page, ?]]

  implicit class JsEffectOps[R, A](e: Eff[R, A]) {
    type _state[Q]  = State[(DB,List[String]),?] |= Q
    type _either[Q] = Either[Page,?] |= Q

    def validationToErrorTree[V](f: V => Validated[String,V]): V => Either[ErrorTree,V] = {
      x => f(x).toEither.leftMap(Tree(_))
    }

    def useForm[OUT, NEWSTACK](
      form: Form[OUT]
    )(
      implicit member: Member.Aux[UniformAsk[OUT,?], R, NEWSTACK],
      stateM: _uniformCore[NEWSTACK],
      eitherM: _either[NEWSTACK],
      action: Action        
    ): Eff[NEWSTACK, A] =
      useForm[Unit,OUT, NEWSTACK]({_ => Html("")}, form)

    def useForm[IN, OUT, NEWSTACK](
      renderTell: IN => Html,
      form: Form[OUT]
    )(
      implicit member: Member.Aux[Uniform[IN,OUT,?], R, NEWSTACK],
      stateM: _uniformCore[NEWSTACK],
      eitherM: _either[NEWSTACK],
      action: Action        
    ): Eff[NEWSTACK, A] = useFormMap(renderTell, _ => form)

    def useFormMap[OUT, NEWSTACK](
      forms: List[String] => Form[OUT]
    )(
      implicit member: Member.Aux[UniformAsk[OUT,?], R, NEWSTACK],
      stateM: _uniformCore[NEWSTACK],
      eitherM: _either[NEWSTACK],
      action: Action        
    ): Eff[NEWSTACK, A] = 
      useFormMap[Unit,OUT,NEWSTACK]({_ => Html("")}, forms)

    def useFormMap[IN, OUT, NEWSTACK](
      renderTell: IN => Html,
      forms: List[String] => Form[OUT]
    )(
      implicit member: Member.Aux[Uniform[IN,OUT,?], R, NEWSTACK],
      stateM: _uniformCore[NEWSTACK],
      eitherM: _either[NEWSTACK],
      action: Action        
    ): Eff[NEWSTACK, A] = e.translate(
      new Translate[Uniform[IN, OUT,?], NEWSTACK] {
        def apply[X](ax: Uniform[IN, OUT,X]): Eff[NEWSTACK, X] = {

          def real: Eff[NEWSTACK,OUT] = {
            val Uniform(keys, tell, default, validation) = ax
            val form = forms(keys)
            val singleKey = keys.mkString(".")
            val formData: Either[ErrorTree,OUT] =
              form.fromNode(keys.last, $("fieldset.uniform"))

            def toTree(input: Option[Encoded]): Tree[String, List[String]] = action match {
              case Submit(_) =>
                extractData($("#content"))
              case Back(_) =>
                input.map(FormUrlEncoded.readString(_).toInputTree).getOrElse(Tree.empty)
            }

            def toDbEntry(input: Option[Encoded]): Option[Either[ErrorTree,OUT]] =
              input.map{d => form.decode(d).flatMap{validationToErrorTree(validation)}}

            for {
              encodedData <- db.encoded.get(keys)
              dataTree = toTree(encodedData)
              dbData = toDbEntry(encodedData)
              breadcrumbs <- breadcrumbs
              va <- (action,dbData) match {
                case (Submit(`keys`),_) =>
                  formData >>= validationToErrorTree(validation) match {
                    case Left(e) =>
                      left[NEWSTACK, Page, OUT](
                        Page(
                          body = form.render(singleKey, dataTree.forestAtPath(keys.last), e).some,
                          errors = e,
                          breadcrumbs = breadcrumbs))
                    case Right(x) =>

                      //put[U, (DB,List[String])]((db + (key -> form.encode(x.asInstanceOf[C])), key :: breadcrumbs)) >>
                      right[NEWSTACK, Page, OUT](x)
                  }
                case (Submit(requestedPage),None) => left[NEWSTACK, Page, OUT](
                  Page(
                    title = singleKey.some,
                    body = form.render(singleKey, None, Tree.empty).some,
                    breadcrumbs=breadcrumbs
                  ))
                case (Submit(requestedPage),Some(Right(x))) if !breadcrumbs.contains(requestedPage) =>
                  crumbPush(keys) >> 
                  right[NEWSTACK, Page, OUT](x)
                case (Submit(requestedPage),Some(Left(e))) =>
                  left[NEWSTACK, Page, OUT](
                    Page(
                      title = singleKey.some,
                      body = form.render(singleKey, None, e).some,
                      errors=e,
                      breadcrumbs=breadcrumbs
                    ))
                    case (Back(`keys`),_) | (Back(_),None | Some(Left(_))) | (Submit(_),Some(Right(_)))=>
                      val err = dbData match {
                        case Some(Left(e)) => e
                        case _ => Tree.empty[String, String]
                      }

                      val dataTree: Tree[String, List[String]] = dbData match {
                        case Some(Right(x)) => Tree(Nil, Map(singleKey -> form.toDataTree(x)))
                        case _              => Tree.empty
                      }
                      println(s"dataTree: ${dataTree.forestAtPath(singleKey)}")
                      left[NEWSTACK, Page, OUT](
                        Page(
                          title = singleKey.some,
                          errors = err,
                          body = form.render(singleKey, dataTree.forestAtPath(singleKey), err).some,
                          breadcrumbs=breadcrumbs
                        ))
                    case (Back(_),Some(Right(x))) => crumbPush(keys) >> right[NEWSTACK, Page, OUT](x)
                  }
            } yield (va)
          }

          real.map(_.asInstanceOf[X])
        }
      }
    )

  }
}
