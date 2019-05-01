package ltbs.uniform.interpreters.playframework

import cats.data._
import cats.implicits._
import ltbs.uniform.{:: => _, _}, web._
import play.api._,mvc._
import play.twirl.api.Html
import reflect.runtime.universe.WeakTypeTag
import scala.concurrent.{ ExecutionContext, Future }
import shapeless._, ops.hlist.Selector

case class AskInput[A](
  id: String,
  t: Html,
  default: Option[A],
  validation: List[List[ValidationRule[A]]],
  customContent: Map[String,(String,List[Any])]
)

trait PlayTell[A] {
  def render(in: A): Html
}

trait PlayAsk[A] {
  def promptUser: AskInput[A] => WebMonad[A]
}

trait PlayInterpreterUF4 extends Compatibility.PlayController {

  def messages(request: Request[AnyContent]): UniformMessages[Html]

  // asks
  protected trait PlayAskHlist[A <: HList] {
    type Supported = A
    def promptUser[Subtype](
      implicit selector : Selector[Supported, Subtype],
      tt: WeakTypeTag[Subtype]
    ): AskInput[Subtype] => WebMonad[Subtype]
  }

  implicit val hnilAsk = new PlayAskHlist[HNil] {
    def promptUser[Subtype](
      implicit s : Selector[Supported, Subtype],
      tt: WeakTypeTag[Subtype]
    ): AskInput[Subtype] => WebMonad[Subtype] = 
      throw new IllegalStateException("not possible!")
  }

  implicit def hConsAsk[H, T <: HList](
    implicit
      hParser: Lazy[PlayAsk[H]],
    tParser: PlayAskHlist[T],
    tth: WeakTypeTag[H]
  ): PlayAskHlist[H :: T] = new PlayAskHlist[H :: T] {
    def promptUser[Subtype](
      implicit selector : Selector[Supported, Subtype],
      ttp: WeakTypeTag[Subtype]
    ): AskInput[Subtype] => WebMonad[Subtype] =
      if (ttp == tth)
        hParser.value.promptUser.asInstanceOf[AskInput[Subtype] => WebMonad[Subtype]] // hParser.value.promptUser.map{_.asInstanceOf[Subtype]}
      else
        tParser.promptUser(selector.asInstanceOf[Selector[tParser.Supported,Subtype]], ttp)
  }

  //tells 
  protected trait PlayTellHlist[A <: HList] {
    type Supported = A
    def render[Subtype](in: Subtype)(
      implicit selector : Selector[Supported, Subtype],
      tt: WeakTypeTag[Subtype]
    ): Html
  }

  implicit val hnilTell = new PlayTellHlist[HNil] {
    def render[Subtype](in: Subtype)(
      implicit selector : Selector[Supported, Subtype],
      tt: WeakTypeTag[Subtype]
    ): Html = throw new IllegalStateException("not possible!")
  }

  implicit def hConsTell[H, T <: HList](
    implicit
      hParser: Lazy[PlayTell[H]],
    tParser: PlayTellHlist[T],
    tth: WeakTypeTag[H]
  ): PlayTellHlist[H :: T] = new PlayTellHlist[H :: T] {
    def render[Subtype](in: Subtype)(
      implicit selector : Selector[Supported, Subtype],
      ttp: WeakTypeTag[Subtype]
    ): Html =
      if (ttp == tth)
        hParser.value.render(in.asInstanceOf[H])
      else
        tParser.render(in)(
          selector.asInstanceOf[Selector[tParser.Supported,Subtype]],
          ttp
        )
  }
  
  def renderForm(
    key: List[String],
    errors: ErrorTree,
    form: Html,
    breadcrumbs: List[String],
    request: Request[AnyContent],
    messages: UniformMessages[Html]
  ): Html

  val log: Logger = Logger("uniform")

  class FuturePlayInterpreter[
    SupportedTell <: HList : PlayTellHlist,
    SupportedAsk <: HList : PlayAskHlist
  ] extends Language[WebMonad, SupportedTell, SupportedAsk] {
    override def interact[Tell: WeakTypeTag, Ask: WeakTypeTag](
      id: String,
      t: Tell,
      default: Option[Ask],
      validation: List[List[ValidationRule[Ask]]],
      customContent: Map[String,(String,List[Any])]
    )(
      implicit selectorTell : Selector[SupportedTell, Tell],
      selectorAsk : Selector[SupportedAsk, Ask]
    ): WebMonad[Ask] = {
      val tellHtml = the[PlayTellHlist[SupportedTell]].render(t)
      val askInput = AskInput[Ask](
        id, tellHtml, default, validation, customContent
      )
      val p = the[PlayAskHlist[SupportedAsk]].promptUser[Ask]
      p(askInput)
    }
  }

}
