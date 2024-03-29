package controllers

import ltbs.uniform._, common.web._, interpreters.playframework._
import play.api.mvc.{Request, AnyContent}
import cats.syntax.semigroup._
import scalatags.Text.all._
import ltbs.uniform.examples.Widgets
import com.github.ghik.silencer.silent

trait HmrcPlayInterpreter
    extends PlayInterpreter[Tag]
    with InferWebAsk[Tag]
    with ScalatagsSupport
    with Widgets
{

  def renderAnd[T](
    pageIn: PageIn[Tag],
    stepDetails: StepDetails[Tag, T],
    members: Seq[(String, Tag)]
  ): Tag = members.toList match {
    case (_, sole) :: Nil => sole
    case many =>
      Widgets.fieldSurround(
        stepDetails.fieldKey,
        stepDetails.tell,
        stepDetails.errors,
        pageIn.messages
      ) (many.map(_._2) :_*)
  }

  def renderOr[T](
    pageIn: PageIn[Tag],
    stepDetails: StepDetails[Tag, T],
    alternatives: Seq[(String, Option[Tag])],
    selected: Option[String]
  ): Tag = Widgets.radios(
    stepDetails.fieldKey,
    stepDetails.tell,
    alternatives.map(_._1),
    selected,
    stepDetails.errors,
    pageIn.messages,
    alternatives.collect{case (k, Some(v)) => (k,v)}.toMap
  )

  def messagesApi: play.api.i18n.MessagesApi
  def messagesForRequest[C <: AnyContent](request: Request[C]): UniformMessages[Tag] =
    {messagesApi.preferred(request).convertMessages() |+| UniformMessages.bestGuess }.map{span(_)}

  implicit def messages(
    implicit request: Request[AnyContent]
  ): UniformMessages[Tag] =
    { messagesApi.preferred(request).convertMessages() |+|
      UniformMessages.bestGuess }.map{span(_)}

  @silent("never used")
  def headerBar(
    serviceName: Option[String],
    navLinks: List[(String, String)],
    activeNavLink: Option[String],
    phase: Option[String],
    messages: UniformMessages[Tag]
  ): Tag = header(cls:="govuk-header ", role:="banner", attr("data-module"):="header")(
    div(cls:="govuk-header__container govuk-width-container") (
      div(cls:="govuk-header__logo")(
        a(href:="#", cls:="govuk-header__link govuk-header__link--homepage")(
          span(cls:="govuk-header__logotype")(
            tag("svg")(role:="presentation", attr("focusable"):="false", cls:="govuk-header__logotype-crown", xmlns:="http://www.w3.org/2000/svg", attr("viewbox"):="0 0 132 97", height:=32, width:=36)(
              tag("path")(
                attr("fill"):="currentColor", attr("fill-rule"):="evenodd", attr("d"):="M25 30.2c3.5 1.5 7.7-.2 9.1-3.7 1.5-3.6-.2-7.8-3.9-9.2-3.6-1.4-7.6.3-9.1 3.9-1.4 3.5.3 7.5 3.9 9zM9 39.5c3.6 1.5 7.8-.2 9.2-3.7 1.5-3.6-.2-7.8-3.9-9.1-3.6-1.5-7.6.2-9.1 3.8-1.4 3.5.3 7.5 3.8 9zM4.4 57.2c3.5 1.5 7.7-.2 9.1-3.8 1.5-3.6-.2-7.7-3.9-9.1-3.5-1.5-7.6.3-9.1 3.8-1.4 3.5.3 7.6 3.9 9.1zm38.3-21.4c3.5 1.5 7.7-.2 9.1-3.8 1.5-3.6-.2-7.7-3.9-9.1-3.6-1.5-7.6.3-9.1 3.8-1.3 3.6.4 7.7 3.9 9.1zm64.4-5.6c-3.6 1.5-7.8-.2-9.1-3.7-1.5-3.6.2-7.8 3.8-9.2 3.6-1.4 7.7.3 9.2 3.9 1.3 3.5-.4 7.5-3.9 9zm15.9 9.3c-3.6 1.5-7.7-.2-9.1-3.7-1.5-3.6.2-7.8 3.7-9.1 3.6-1.5 7.7.2 9.2 3.8 1.5 3.5-.3 7.5-3.8 9zm4.7 17.7c-3.6 1.5-7.8-.2-9.2-3.8-1.5-3.6.2-7.7 3.9-9.1 3.6-1.5 7.7.3 9.2 3.8 1.3 3.5-.4 7.6-3.9 9.1zM89.3 35.8c-3.6 1.5-7.8-.2-9.2-3.8-1.4-3.6.2-7.7 3.9-9.1 3.6-1.5 7.7.3 9.2 3.8 1.4 3.6-.3 7.7-3.9 9.1zM69.7 17.7l8.9 4.7V9.3l-8.9 2.8c-.2-.3-.5-.6-.9-.9L72.4 0H59.6l3.5 11.2c-.3.3-.6.5-.9.9l-8.8-2.8v13.1l8.8-4.7c.3.3.6.7.9.9l-5 15.4v.1c-.2.8-.4 1.6-.4 2.4 0 4.1 3.1 7.5 7 8.1h.2c.3 0 .7.1 1 .1.4 0 .7 0 1-.1h.2c4-.6 7.1-4.1 7.1-8.1 0-.8-.1-1.7-.4-2.4V34l-5.1-15.4c.4-.2.7-.6 1-.9zM66 92.8c16.9 0 32.8 1.1 47.1 3.2 4-16.9 8.9-26.7 14-33.5l-9.6-3.4c1 4.9 1.1 7.2 0 10.2-1.5-1.4-3-4.3-4.2-8.7L108.6 76c2.8-2 5-3.2 7.5-3.3-4.4 9.4-10 11.9-13.6 11.2-4.3-.8-6.3-4.6-5.6-7.9 1-4.7 5.7-5.9 8-.5 4.3-8.7-3-11.4-7.6-8.8 7.1-7.2 7.9-13.5 2.1-21.1-8 6.1-8.1 12.3-4.5 20.8-4.7-5.4-12.1-2.5-9.5 6.2 3.4-5.2 7.9-2 7.2 3.1-.6 4.3-6.4 7.8-13.5 7.2-10.3-.9-10.9-8-11.2-13.8 2.5-.5 7.1 1.8 11 7.3L80.2 60c-4.1 4.4-8 5.3-12.3 5.4 1.4-4.4 8-11.6 8-11.6H55.5s6.4 7.2 7.9 11.6c-4.2-.1-8-1-12.3-5.4l1.4 16.4c3.9-5.5 8.5-7.7 10.9-7.3-.3 5.8-.9 12.8-11.1 13.8-7.2.6-12.9-2.9-13.5-7.2-.7-5 3.8-8.3 7.1-3.1 2.7-8.7-4.6-11.6-9.4-6.2 3.7-8.5 3.6-14.7-4.6-20.8-5.8 7.6-5 13.9 2.2 21.1-4.7-2.6-11.9.1-7.7 8.8 2.3-5.5 7.1-4.2 8.1.5.7 3.3-1.3 7.1-5.7 7.9-3.5.7-9-1.8-13.5-11.2 2.5.1 4.7 1.3 7.5 3.3l-4.7-15.4c-1.2 4.4-2.7 7.2-4.3 8.7-1.1-3-.9-5.3 0-10.2l-9.5 3.4c5 6.9 9.9 16.7 14 33.5 14.8-2.1 30.8-3.2 47.7-3.2z"),
              tag("image")( src:="/assets/images/govuk-logotype-crown.png", cls:="govuk-header__logotype-crown-fallback-image"),
            ),
            span( cls:="govuk-header__logotype-text")(messages("GOV.UK"))
          )
        )
      )
    )
  )

  def phaseTag(phaseName: String, messages: UniformMessages[Tag]): Tag =
    div(cls:="govuk-width-container")(
      div(cls:="govuk-phase-banner")(
        p(cls:="govuk-phase-banner__content")(
          strong(cls:="govuk-tag govuk-phase-banner__content__tag ")(phaseName),
          span(cls:="govuk-phase-banner__text")(
            messages("very.old.service"),
            a( cls:="govuk-link", href:="#")(messages("feedback")),
            messages("help.improve")
          )
        )
      )
    )

  def hmrcFooter(
    navLinks: List[(Int, String, List[(String, String)])],
    metaLinks: List[(String, String)],
    messages: UniformMessages[Tag]
  ): Tag = footer(cls:="govuk-footer", role:="contentinfo")(
    div(cls:="govuk-width-container")(
      if(navLinks.nonEmpty) {
        div(cls:="govuk-footer__navigation")(
          navLinks.map{case (cols, title, links) =>
            div(cls:= "govuk-footer__section")(
              h2(cls:= "govuk-footer__heading govuk-heading-m")(messages(title)),
              ul(cls:= s"govuk-footer__list govuk-footer__list--columns-$cols") (
                links.map{ case (link,url) =>
                  li(cls:="govuk-footer__list-item")(
                    a(cls:="govuk-footer__link", href:=url) (messages(link))
                  )
                }
              )
            )
          }
        )
      },
      if(navLinks.nonEmpty && metaLinks.nonEmpty) hr( cls:="govuk-footer__section-break"),
      div(cls:="govuk-footer__meta")(
        div(cls:="govuk-footer__meta-item govuk-footer__meta-item--grow") (
          h2(cls:="govuk-visually-hidden")(messages("support-links")),
          if(metaLinks.nonEmpty) (
            ul(cls:="govuk-footer__inline-list")(
              metaLinks.map{case (link,url) =>
               li( cls:="govuk-footer__inline-list-item")(
                 a (cls:="govuk-footer__link", href:=url)(messages(link))
               )
              }
            )
          ),
          tag("svg")(role:="presentation", attr("focusable"):="false", cls:="govuk-footer__licence-logo", xmlns:="http://www.w3.org/2000/svg",attr("viewbox"):="0 0 483.2 195.7",height:=17, width:=41)(
            tag("path")(attr("fill"):="currentColor", attr("d"):="M421.5 142.8V.1l-50.7 32.3v161.1h112.4v-50.7zm-122.3-9.6A47.12 47.12 0 0 1 221 97.8c0-26 21.1-47.1 47.1-47.1 16.7 0 31.4 8.7 39.7 21.8l42.7-27.2A97.63 97.63 0 0 0 268.1 0c-36.5 0-68.3 20.1-85.1 49.7A98 98 0 0 0 97.8 0C43.9 0 0 43.9 0 97.8s43.9 97.8 97.8 97.8c36.5 0 68.3-20.1 85.1-49.7a97.76 97.76 0 0 0 149.6 25.4l19.4 22.2h3v-87.8h-80l24.3 27.5zM97.8 145c-26 0-47.1-21.1-47.1-47.1s21.1-47.1 47.1-47.1 47.2 21 47.2 47S123.8 145 97.8 145")
          ),
          span( cls:="govuk-footer__licence-description")(
            messages("all-content-available"),
            a( cls:="govuk-footer__link", href:="https://www.nationalarchives.gov.uk/doc/open-government-licence/version/3/", rel:="license")(
              messages("ogl3")
            ),
            messages("except-where-otherwise-stated")
          )
        ),
        div( cls:="govuk-footer__meta-item")(
          a( cls:="govuk-footer__link govuk-footer__copyright-logo", href:="https://www.nationalarchives.gov.uk/information-management/re-using-public-sector-information/uk-government-licensing-framework/crown-copyright/")("© ", messages("crown-copyright"))
        )
      )
    )
  )

  def pageChrome(
    key: List[String],
    errors: ErrorTree,
    tellAndAsk: Option[Tag],
    breadcrumbs: List[String],
    request: Request[AnyContent],
    messages: UniformMessages[Tag]
  ): Tag = {

    import play.filters.csrf._
    import play.filters.csrf.CSRF.Token
    val Token(_,csrf: String) = CSRF.getToken(request).get

    html(title := "Uniform Play :: Example Service")(
      head(
        meta(charset:="utf-8")(),
        meta(name:="viewport", content:="width=device-width, initial-scale=1"),
        link(rel:="stylesheet", tpe:="text/css", href:="/assets/govuk-frontend-2.4.0.min.css"),
        link(rel:="stylesheet", tpe:="text/css", href:="/assets/uniform-link-buttons.css"),
        link(rel:="stylesheet", tpe:="text/css", href:="/assets/uniform.css"),
        script(tpe:="text/javascript", src:="/assets/jquery-3.3.1.min.js"),
        script(tpe:="text/javascript", src:="/assets/govuk-frontend-2.4.0.min.js"),
        script(tpe:="text/javascript", src:="/assets/show-hide-content.js"),
        meta(name:="theme-color", content:="#0b0c0c"),
        link(rel:="shortcut icon", attr("sizes"):="16x16 32x32 48x48", href:="/assets/images/favicon.ico", tpe:="image/x-icon"),
        link(rel:="mask-icon", href:="/assets/images/govuk-mask-icon.svg", color:="#0b0c0c"),
        meta(name:="robots", content:="noindex, nofollow"),
        meta(attr("property"):="og:image", content:="/assets/images/govuk-opengraph-image.png")
      ),
      body(cls:= "govuk-template__body ")(
        a(href:= "#main-content", cls:="govuk-skip-link")("Skip to main content"),
        headerBar(Some("Uniform Play Interpreter Example"), Nil, None, Some("EXPERIMENTAL"), messages),
        phaseTag("tudor", messages),
        div(cls:="govuk-width-container")(
          tag("main")(cls:= "govuk-main-wrapper ", id:="main-content", role:="main")(
            div(id:="mainBody") (
              breadcrumbs.drop(1).headOption.map{ back =>
                a (href:=back, cls:="govuk-back-link")(messages({back :+ "back"}.mkString(".")))
              },
              div(cls:="govuk-width-container")(
                tag("main")(cls:="govuk-main-wrapper ", id:="main-content", role:="main")(
                  h1(cls:="govuk-heading-xl")(messages(key.mkString("."))),
                  messages.get((key :+ "hint").mkString(".")).map { hintMsg =>
                    span( id := (key :+ "hint").mkString("_"), cls := "govuk-hint") (hintMsg)
                  }
                )
              ),
              tellAndAsk.map { a =>
                form(method:="post")(
                  input(tpe:="hidden", name:="csrfToken", value:=csrf),
                  a
                )
              }
            )
          )
        ),
        div(breadcrumbs.toString),
        hmrcFooter(Nil,Nil,messages)
      )
    )
  }
}
