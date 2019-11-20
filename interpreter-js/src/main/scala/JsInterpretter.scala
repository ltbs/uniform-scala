package ltbs.uniform
package interpreters.js

import common.web._

import concurrent._
import org.querki.jquery._

abstract class JsInterpreter[Html](
  implicit ec: ExecutionContext
) extends GenericWebInterpreter[Html] {

  def renderFrame(
    key: List[String], 
    frame: JQuery, 
    htmlForm: Html,
    errors: ErrorTree,
    messages: UniformMessages[Html]
  ): Future[Unit]

  def renderState(stateOut: DB): String = {

    def renderInput(key: String, i: Input): String = {

      val topLi = i.valueAtRoot match {
        case Some(rootValue) =>       s"""<li><strong>$key</strong> : ${rootValue.mkString(",")}"""
        case None => s"""<li><strong>$key</strong>"""
      }

      val elements = i.keys.collect {
        case single::_ => renderInput(single, i / single)
      }

      topLi ++ {if (elements.nonEmpty) {
        "<ul>" ++ elements.mkString ++ "</ul>"
      } else {
        ""
      }} ++ "</li>"
    }

    "<ul>" ++ stateOut.map {
      case (pagePath, valueEncoded) =>
        val Right(value) = Input.fromUrlEncodedString(valueEncoded)
        renderInput(pagePath.mkString(" / "), value)
    }.mkString ++ "</ul>"
  }
  
  def renderCrumbs(crumbsOut: Path): String =
    "<ol>" ++
      crumbsOut.reverse.map { pathElement =>
        s"""<li><a onclick="">${pathElement.mkString(" / ")}</a></li>"""
      }.mkString ++ "</ol>"


  case class JsRunner[A](
    wm: WebMonad[A, Html],
    selector: JQuery,
    var db: DB = DB.empty,
    var crumbs: Path = Nil,
    purgeStateUponCompletion: Boolean = false,
    diagnostics: Boolean = false
  )(
    f: A => Future[Result]
  ) {

    def goBack(): Future[Unit] = crumbs match {
      case (_::last::_) => 
        run(PageIn(last, Nil, None, db))
      case _ =>
        Future.successful(())
    }

    def submit(): Future[Unit] = {
      val dataStringEncoded = selector.serialize()
      val dataInput = Input.fromUrlEncodedString(dataStringEncoded)
      run(PageIn(crumbs.head, Nil, dataInput.toOption, db))
    }

    def run(
      request: PageIn
    ): Future[Result] = wm(request) flatMap {
      case common.web.PageOut(path, dbOut, pageOut) =>

        db = dbOut
        crumbs = path

        if (diagnostics) {
          $("#state").html(renderState(db))
          $("#crumbs").html(renderCrumbs(crumbs))          
        }

        pageOut match {
          case AskResult.GotoPath(targetPath) =>
            run(request.copy(targetId = targetPath, path = Nil, request = None, state = db))
          case AskResult.Payload(html, errors, messagesOut, _) =>
            renderFrame(request.targetId, selector, html, errors, messagesOut)
          case AskResult.Success(result) =>
            f(result) map { _ => 
              if (purgeStateUponCompletion) {db = DB.empty}
              ()
            }
        }        
    }
    run(PageIn(Nil, Nil, None, db))
  }

}

