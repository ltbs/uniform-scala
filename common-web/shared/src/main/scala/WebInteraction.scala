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

  implicit def fromTellAndAsk[Html, T, A](
    implicit gwt: WebTell[Html, T],
    ff: WebAsk[Html, A]
  ): WebInteraction[Html, T, A] = new PostAndGetPage[Html, T, A] {
    def codec: Codec[A] = ff.codec
    override def getPage(
      pageIn: PageIn[Html],
      key: List[String],
      tell: Option[T],
      existing: Input,
      rule: Rule[A]
    )(implicit ec: ExecutionContext): Option[Html] = {
      val tellHtml = tell.flatMap(gwt.render(_, key.last, pageIn.messages))
      ff.render(pageIn, StepDetails[Html, A](key, key, tellHtml, existing, ErrorTree.empty, rule))
    }

    override def postPage(
      pageIn: PageIn[Html],      
      key: List[String],
      tell: Option[T],
      request: Input,
      rule: Rule[A],      
      errors: ErrorTree
    )(implicit ec: ExecutionContext): Option[Html] = {
      val tellHtml = tell.flatMap(gwt.render(_, key.last, pageIn.messages))
      ff.render(pageIn, StepDetails[Html, A](key, key, tellHtml, request, errors, rule))
    }
  }

  implicit def fromTellNothing[Html, T](
    implicit gwt: WebTell[Html, T],
    ff: WebAsk[Html, Nothing]    
  ): WebInteraction[Html, T, Nothing] = new PostAndGetPage[Html, T, Nothing] {
    def codec: Codec[Nothing] = ff.codec
    override def getPage(
      pageIn: PageIn[Html],      
      key: List[String],
      tell: Option[T],
      existing: Input,
      rule: Rule[Nothing]
    )(implicit ec: ExecutionContext): Option[Html] = {
      val tellHtml = tell.flatMap(gwt.render(_, key.last, pageIn.messages))
      ff.render(pageIn, StepDetails[Html, Nothing](key, key, tellHtml, existing, ErrorTree.empty, rule))
    }

    override def postPage(
      pageIn: PageIn[Html],      
      key: List[String],
      tell: Option[T],
      request: Input,
      rule: Rule[Nothing],      
      errors: ErrorTree
    )(implicit ec: ExecutionContext): Option[Html] =
      getPage(pageIn, key, tell, request, rule)
  }

  implicit def fromAskUnit[Html, A](
    implicit ff: WebAsk[Html, A]
  ): WebInteraction[Html, Unit, A] = new PostAndGetPage[Html, Unit, A] {
    def codec: Codec[A] = ff.codec
    override def getPage(
      pageIn: PageIn[Html],      
      key: List[String],
      tell: Option[Unit],
      existing: Input,
      rule: Rule[A]
    )(implicit ec: ExecutionContext): Option[Html] = {
      ff.render(pageIn, StepDetails[Html, A](key, key, None, existing, ErrorTree.empty, rule))
    }

    override def postPage(
      pageIn: PageIn[Html],      
      key: List[String],
      tell: Option[Unit],
      request: Input,
      rule: Rule[A],
      errors: ErrorTree
    )(implicit ec: ExecutionContext): Option[Html] = {
      ff.render(pageIn, StepDetails[Html, A](key, key, None, request, errors, rule))
    }
  }


}

