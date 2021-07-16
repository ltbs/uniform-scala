package ltbs.uniform
package common.web
import validation.Rule
import scala.concurrent.ExecutionContext

trait WebInteraction[Html, T, A] {
  def apply(
    id: String,
    tell: Option[T],
    defaultIn: Option[A],
    validationIn: Rule[A],
    customContent: Map[String,(String,List[Any])]
  ): WebMonad[Html, A]
}

object WebInteraction {

  class StandardTellAndAskForm[Html, T, A](
    gwt: WebTell[Html, T],
    ff: WebAsk[Html, A]
  ) extends PostAndGetPage[Html, T, A] {

    def codec: Codec[A] = ff.codec
    override def getPage(
      key: List[String],
      tell: Option[T],
      state: DB,
      existing: Input,
      breadcrumbs: Breadcrumbs,
      messages: UniformMessages[Html]
    )(implicit ec: ExecutionContext): Option[Html] = {
      val tellHtml = tell.flatMap(gwt.render(_, key.last, messages))
      ff.render(key, key, tellHtml, breadcrumbs, existing, ErrorTree.empty, messages)
    }

    override def postPage(
      key: List[String],
      tell: Option[T],
      state: DB,
      request: Input,
      errors: ErrorTree,
      breadcrumbs: Breadcrumbs,
      messages: UniformMessages[Html]
    )(implicit ec: ExecutionContext): Option[Html] = {
      val tellHtml = tell.flatMap(gwt.render(_, key.last, messages))
      ff.render(key, key, tellHtml, breadcrumbs, request, errors, messages)
    }
  }

  implicit def fromTellAndAsk[Html, T, A](
    implicit gwt: WebTell[Html, T],
    ff: WebAsk[Html, A]
  ): WebInteraction[Html, T, A] = new PostAndGetPage[Html, T, A] {
    def codec: Codec[A] = ff.codec
    override def getPage(
      key: List[String],
      tell: Option[T],
      state: DB,
      existing: Input,
      breadcrumbs: Breadcrumbs,
      messages: UniformMessages[Html]
    )(implicit ec: ExecutionContext): Option[Html] = {
      val tellHtml = tell.flatMap(gwt.render(_, key.last, messages))
      ff.render(key, key, tellHtml, breadcrumbs, existing, ErrorTree.empty, messages)
    }

    override def postPage(
      key: List[String],
      tell: Option[T],
      state: DB,
      request: Input,
      errors: ErrorTree,
      breadcrumbs: Breadcrumbs,
      messages: UniformMessages[Html]
    )(implicit ec: ExecutionContext): Option[Html] = {
      val tellHtml = tell.flatMap(gwt.render(_, key.last, messages))
      ff.render(key, key, tellHtml, breadcrumbs, request, errors, messages)
    }
  }

  implicit def fromAsk[Html, A](
    implicit ff: WebAsk[Html, A]
  ): WebInteraction[Html, Unit, A] = new PostAndGetPage[Html, Unit, A] {
    def codec: Codec[A] = ff.codec
    override def getPage(
      key: List[String],
      tell: Option[Unit],
      state: DB,
      existing: Input,
      breadcrumbs: Breadcrumbs,
      messages: UniformMessages[Html]
    )(implicit ec: ExecutionContext): Option[Html] = {
      ff.render(key, key, None, breadcrumbs, existing, ErrorTree.empty, messages)
    }

    override def postPage(
      key: List[String],
      tell: Option[Unit],
      state: DB,
      request: Input,
      errors: ErrorTree,
      breadcrumbs: Breadcrumbs,
      messages: UniformMessages[Html]
    )(implicit ec: ExecutionContext): Option[Html] = {
      ff.render(key, key, None, breadcrumbs, request, errors, messages)
    }
  }


}

