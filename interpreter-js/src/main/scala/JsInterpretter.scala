package ltbs.uniform.prototype

import cats.data._
import org.atnos.eff._
import cats.implicits._
import org.atnos.eff.all._
import org.atnos.eff.syntax.all._
import org.querki.jquery._
import ltbs.uniform._
import cats.data.State
import ltbs.uniform._
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

  trait Form[T] {
    def render(
      key: String,
      existing: Option[Tree[String,List[String]]],
      errors: ErrorTree,
      tell: Html
    ): String

    def fromNode(
      key: String,
      fieldSet: JQuery
    ): Either[ErrorTree, T]

    def encode(in: T): Encoded

    def decode(out: Encoded): Either[ErrorTree,T]

    def toDataTree(in: T): Tree[String,List[String]]
  }

  type JsStack = Fx.fx2[State[UniformCore, ?], Either[Page, ?]]

  implicit class JsEffectOps[STACK, A](e: Eff[STACK, A]) {
    type _state[Q]  = State[(DB,List[String]),?] |= Q
    type _either[Q] = Either[Page,?] |= Q

    def validationToErrorTree[V](f: V => Validated[String,V]): V => Either[ErrorTree,V] = {
      x => f(x).toEither.leftMap(Tree(_))
    }

    def useForm[OUT, NEWSTACK](
      form: Form[OUT]
    )(
      implicit member: Member.Aux[UniformAsk[OUT,?], STACK, NEWSTACK],
      stateM: _uniformCore[NEWSTACK],
      eitherM: _either[NEWSTACK],
      action: Action
    ): Eff[NEWSTACK, A] =
      useForm[Unit,OUT, NEWSTACK]({_ => Html("")}, form)

    def useForm[IN, OUT, NEWSTACK](
      renderTell: IN => Html,
      form: Form[OUT]
    )(
      implicit member: Member.Aux[Uniform[IN,OUT,?], STACK, NEWSTACK],
      stateM: _uniformCore[NEWSTACK],
      eitherM: _either[NEWSTACK],
      action: Action
    ): Eff[NEWSTACK, A] = useFormMap(renderTell, _ => form)

    def useFormMap[OUT, NEWSTACK](
      forms: List[String] => Form[OUT]
    )(
      implicit member: Member.Aux[UniformAsk[OUT,?], STACK, NEWSTACK],
      stateM: _uniformCore[NEWSTACK],
      eitherM: _either[NEWSTACK],
      action: Action
    ): Eff[NEWSTACK, A] =
      useFormMap[Unit,OUT,NEWSTACK]({_ => Html("")}, forms)

    def useFormMap[IN, OUT, NEWSTACK](
      renderTell: IN => Html,
      forms: List[String] => Form[OUT]
    )(
      implicit member: Member.Aux[Uniform[IN,OUT,?], STACK, NEWSTACK],
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
                          body = Some{
                            renderTell(tell).toString |+|
                            form.render(keys.last, dataTree.forestAtPath(keys.last), e, renderTell(tell))
                          },
                          errors = e,
                          breadcrumbs = breadcrumbs))
                    case Right(x) =>
                      (db.encoded(keys) = form.encode(x)) >>
                      crumbPush(keys) >>
                      right[NEWSTACK, Page, OUT](x)
                  }
                case (Submit(requestedPage),None) => left[NEWSTACK, Page, OUT](
                  Page(
                    title = singleKey.some,
                    body = Some(
                        form.render(keys.last, None, Tree.empty, renderTell(tell))
                    ),
                    breadcrumbs=breadcrumbs
                  ))
                case (Submit(requestedPage),Some(Right(x))) if !breadcrumbs.contains(requestedPage) =>
                  crumbPush(keys) >>
                  right[NEWSTACK, Page, OUT](x)
                case (Submit(requestedPage),Some(Left(e))) =>
                  left[NEWSTACK, Page, OUT](
                    Page(
                      title = singleKey.some,
                      body = Some(
                          form.render(keys.last, None, e, renderTell(tell))
                      ),
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
                          body = Some(
                              form.render(keys.last, dataTree.forestAtPath(singleKey), err, renderTell(tell))
                          ),
                          breadcrumbs=breadcrumbs
                        ))
                case (Back(_),Some(Right(x))) =>
                  crumbPush(keys) >> right[NEWSTACK, Page, OUT](x)
              }
            } yield (va)
          }

          real.map(_.asInstanceOf[X])
        }
      }
    )

    def delist[OUT, NEWSTACK, INNER: _uniformCore](
      subJourneyP: (List[OUT], Option[OUT]) => Eff[INNER, OUT],
    )(
      implicit member: Member.Aux[UniformAsk[List[OUT],?], STACK, NEWSTACK],
      stateM: _uniformCore[NEWSTACK],
      listingPage: _uniform[List[OUT], ListControl, NEWSTACK],
      confirmationPage: _uniform[OUT, Boolean, INNER],
      parser: DataParser[List[OUT]],
      f: IntoPoly[INNER,NEWSTACK]
    ): Eff[NEWSTACK,A] = {

      def w(allElements: List[OUT], removalCandidate: OUT): Eff[INNER, Boolean] =
        dialogue[OUT,Boolean]("confirm")(removalCandidate).in[INNER]

      delistWithCustomRemove[OUT, NEWSTACK, INNER](
        subJourneyP,
        w
      )
    }

    def delistWithCustomRemove[OUT, NEWSTACK, INNER](
      subJourneyP: (List[OUT], Option[OUT]) => Eff[INNER, OUT],
      removeConfirmation: (List[OUT], OUT) => Eff[INNER, Boolean]
    )(
      implicit member: Member.Aux[UniformAsk[List[OUT],?], STACK, NEWSTACK],
      stateM: _uniformCore[NEWSTACK],
      listingPage: _uniform[List[OUT], ListControl, NEWSTACK],
      parser: DataParser[List[OUT]],
      f: IntoPoly[INNER,NEWSTACK]
    ): Eff[NEWSTACK,A] = e.translate(
      new Translate[UniformAsk[List[OUT],?], NEWSTACK] {

        def serialise(in: List[OUT]): String = FormUrlEncoded.fromInputTree(parser.unbind(in)).writeString
        def deserialise(in: String): List[OUT] = parser.bind(FormUrlEncoded.readString(in).toInputTree) match {
          case Left(t) if t.isEmpty => Nil
          case Left(e) => throw new IllegalStateException(e.toString)
          case Right(r) => r
        }

        def apply[X](ax: UniformAsk[List[OUT],X]): Eff[NEWSTACK, X] = {

          def real: Eff[NEWSTACK,List[OUT]] = ax match {
            case Uniform(id, tell, default, validation) =>

              def read: Eff[NEWSTACK, Option[List[OUT]]] =
                db.encoded.get(id :+ "__data").map(_.map(deserialise))

              def write(in: List[OUT]): Eff[NEWSTACK, Unit] =
                db.encoded(id :+ "__data") = serialise(in)

              def process(elements: List[OUT]): Eff[NEWSTACK,List[OUT]] = {
                uniformP[List[OUT],ListControl,NEWSTACK](id, elements) >>=
                {_ match {
                  case ltbs.uniform.web.Continue =>
                    Eff.pure[NEWSTACK,List[OUT]](elements)

                  case AddAnother =>
                    subjourney("add") {
                      subJourneyP(elements, None).into[NEWSTACK]
                    } >>= {x =>
                      db.remove(id) >>
                      db.removeRecursive(id.dropRight(1) :+ "add") >>
                      write(elements :+ x) >>
                      process(elements :+ x)}

                  case Edit(ordinal) =>
                    subjourney("edit") {
                      subJourneyP(elements, elements.get(ordinal)).into[NEWSTACK]
                    } >>= {x =>
                      db.remove(id) >>
                      db.removeRecursive(id.dropRight(1) :+ "edit") >>
                      write(elements.replace(ordinal, x)) >>
                      process(elements.replace(ordinal, x))}

                  case Delete(ordinal) =>
                    subjourney("delete") {
                      removeConfirmation(elements, elements(ordinal)).into[NEWSTACK]
                    } >>= {
                      if (_) {
                        write(elements.delete(ordinal)) >>
                        db.removeRecursive(id.dropRight(1) :+ "delete") >>
                        db.remove(id) >>
                        process(elements.delete(ordinal))
                      } else
                        db.removeRecursive(id.dropRight(1) :+ "delete") >>
                        db.remove(id) >>
                        process(elements)

                    }
                }}
              }

              read >>= { state => process(state.orElse(default).getOrElse(Nil)) }

          }
          real.map{_.asInstanceOf[X]}
        }
      }
    )

  }

  def listingTable[E](
    key: String,
    render: (String, List[(Html, Option[Html], Option[Html])], Int, Int, Messages) => Html,
    elementToHtml: E => Html,
    messages: Messages
  )(elements: List[E]): Html = {

    def edit(i: Int) = Html(
      s"""|<a href="#" onclick="
          |$$('input[name=&quot;$key.Edit.ordinal&quot;]').val('$i');
          |$$('[name=&quot;$key&quot;]').removeAttr('checked');
          |$$('input[name=$key][value=Edit]').prop('checked', true);
          |saveAndContinue('$key');
          |">Edit</a>
          |""".stripMargin)

    def delete(i: Int) = Html(
      s"""|<a href="#" onclick="
          |$$('input[name=&quot;$key.Delete.ordinal&quot;]').val('$i');
          |$$('[name=&quot;$key&quot;]').removeAttr('checked');
          |$$('input[name=$key][value=Delete]').prop('checked', true);
          |saveAndContinue('$key');
          |">Delete</a>
          |""".stripMargin)

    render(key, elements.zipWithIndex.map{
      case (x,i) => (elementToHtml(x), Some(edit(i)), Some(delete(i)))
    }, 0, Int.MaxValue, messages)

  }
}
