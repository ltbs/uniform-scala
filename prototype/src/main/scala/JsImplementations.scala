package ltbs.uniform.prototype

import cats.data.NonEmptyList
import scala.scalajs.runtime.UndefinedBehaviorError
import ltbs.uniform._
import org.querki.jquery._
import JsInterpreter._
import cats.data.{ValidatedNel, Validated}
import cats.implicits._
import cats.syntax.cartesian._

object JsImplementations {

  def radioForm[T](options: T*)(implicit messages: MessagesProvider) = new Form[T] {

    val lookup = options.map{v => (v.toString, v)}.toMap

    def render(existing: ValidatedData[T]): String = {
      val optHtml = options.map{_.toString}.map{ key =>
        s"""|<div class="multiple-choice">
            |  <input id="changeType-$key" name="changeType" value="$key" type="radio">
            |  <label for="changeType-$key" class="block-label less-padding">${messages.span(key)}</label>
            |</div>""".stripMargin
      }

      s"""|<fieldset id="changeType" class="form-field-group">
          |  ${optHtml.mkString("\n")}
          |</fieldset>""".stripMargin
    }

    def fromNode(fieldSet: JQuery): ValidatedNel[ValidationError,T] = {

      Validated.catchOnly[UndefinedBehaviorError](
        $("input[name=changeType]:checked").valueString
      ).andThen { e: String => 
        Validated.catchOnly[NoSuchElementException](
          lookup(e)
        )
      }.bimap(_ => NonEmptyList(messages.span("error.radio-form.choose-option"),Nil), identity)
    }

    def encode(in: T): Encoded = options.indexOf(in).toString
    def decode(out: Encoded): T = options(out.toInt)
  }

  def booleanForm(implicit messages: MessagesProvider): Form[Boolean] = radioForm(true, false)

  def litresForm(implicit messages: MessagesProvider): Form[(Long, Long)] = new Form[(Long,Long)] {
    def render(existing: ValidatedData[(Long,Long)]): String = {
      s"""|<div class="form-group form-litres-3-4">
          |  <div class="form-group form-copackQty.lower  ">
          |    <label for="copackQty.lower">
          |      <span class="form-label">${messages.span("sdil.litreage.lowRate")}</span>
          |    </label>
          |    <input class="form-control
          |                  input--no-spinner volume " id="lowerQty" name="copackQty.lower"
          |                  value="${existing.flatMap(_.toOption.map{_._1}).getOrElse("")}" type="text" />
          |  </div>
          |</div>
          |<div class="form-group form-litres-3-4">
          |  <div class="form-group form-copackQty.higher  ">
          |    <label for="copackQty.higher">
          |      <span class="form-label">${messages.span("sdil.litreage.highRate")}</span>
          |    </label>
          |    <input class="form-control
          |                  input--no-spinner volume " id="higherQty" name="copackQty.higher"
          |                  value="${existing.flatMap(_.toOption.map{_._2}).getOrElse("")}" type="text" />
          |  </div>
          |</div>
          |<details class="padding-bottom-10">
          |  <summary role="button" aria-expanded="false">
          |    <span class="summary" onclick="ga('send', 'event', 'helpLinks', 'help', 'What are the bands?')">
          |      ${messages.span("sdil.litreage.rates.help")}
          |    </span>
          |  </summary>
          |  <div class="panel panel-border-narrow margin-bottom-1" id="details-content-0">
          |    <p>${messages.span("sdil.litreage.rates.content.p1")}.</p>
          |    <p>${messages.span("sdil.litreage.rates.content.p2")}.</p>
          |  </div>
          |</details>
          |<details class="padding-bottom-10">
          |  <summary role="button" aria-expanded="false">
          |    <span class="summary" onclick="ga('send', 'event', 'helpLinks', 'help', 'I don’t know these volumes')">
          |      ${messages.span("sdil.litreage.volumes.help")}
          |    </span>
          |  </summary>
          |  <div class="panel panel-border-narrow margin-bottom-1" id="details-content-0">
          |    <p>${messages.span("sdil.litreage.volumes.content.p1")}.</p>
          |  </div>
          |</details>""".stripMargin
    }

    def numberField(fieldSet: JQuery, selector: String) = {
      val element = fieldSet.find(selector)
      println(s"element:$element")
      println(s"element.valueString:${element.value()}")
      Validated.catchOnly[NumberFormatException](fieldSet.find(selector).valueString.replaceAll(",","").toLong)
        .bimap(_ => NonEmptyList("Please enter a valid number", Nil), identity)
    }

    def fromNode(fieldSet: JQuery): ValidatedNel[ValidationError, (Long,Long)] = {
      (numberField(fieldSet, "#lowerQty"),numberField(fieldSet, "#higherQty")).mapN((_,_))
    }

    def encode(in: (Long,Long)): Encoded = s"${in._1},${in._2}"
    def decode(out: Encoded): (Long,Long) = {
      val (l::h::_) = out.split(",").map{_.toLong}.toList
      (l,h)
    }
  }
}
