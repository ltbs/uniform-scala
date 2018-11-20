package controllers

import cats.kernel.Monoid
import javax.inject._
import play.api._
import play.api.data._, Forms._
import play.api.mvc._
import play.twirl.api.Html
import scala.util.Try

import ltbs.uniform.webmonad._
import org.atnos.eff._, all.{none => _,_}, syntax.all._, future._
import cats.data._
import cats.implicits._
import org.atnos.eff.syntax.future._

import scala.concurrent._, duration._
import scala.concurrent.ExecutionContext.Implicits.global
import cats.data.Validated
import java.time.{LocalDate => Date}
import java.io.File
import ltbs.uniform.gformsparser._
import play.api.i18n._
import ofsted.Programs.cs3

trait BaseController extends Controller with PlayInterpreter with I18nSupport {

}
