package ltbs.uniform.gformsparser

import cats.implicits._
import scala.reflect.NameTransformer.{ encode => nameEncode }

object TemplateToSourceCode {

  def messagesToStatement(in: Map[String,String]): String = {
    import scala.reflect.runtime.universe._
    show(q"$in")
  }

  def extractMessages(template: GformTemplate, config: Config): Map[String,String] =
    template.sections.flatMap { case section =>
      val sectionId = template.deduplicatedSectionId(section)
      (s"${sectionId}.heading" -> section.title) ::
      section.infoFields.zipWithIndex.map {
        case (i,index) => (s"${sectionId}.info${index+1}" -> i.infoText)
      } ++
      section.fields.flatMap { field =>
        val helpText = field.helpText.map{t => (s"${sectionId}.${field.id}.hint" -> t)}
        val label = Some(field.label).filter(_.trim.nonEmpty).map{t => (s"${sectionId}.${field.id}.label" -> t)}
        List(helpText,label).flatten
      }
    }.toMap

  def apply(template: GformTemplate, config: Config): (String, Option[String], Option[String]) = {

    val sections: List[Section] = template.allSections
    def id(sec: Section) = template.deduplicatedSectionId(sec)

    def dataConstruct(cf: ChoiceField): List[String] = {
      val name = cf.id.trim.split(" ").toList.camel
      List(
        s"sealed trait $name",
        s"object $name {\n    " ++ cf.choices.map{ option =>
          val optName = option.trim.split(" ").toList.camel
          s"case object $optName extends $name"
        }.mkString("\n    ") ++ "\n  }",
        s"""lazy implicit val ${"eq" ++ name} = cats.kernel.Eq.fromUniversalEquals[$name]"""
      )
    }

    def _sectionDataType(s: Section): (String, String, List[String]) = {
      val (tellsRaw,asksRaw) = s.fields.partition { _.isInstanceOf[InfoField]}

      // we probably only need to use a tell if we have an expression
      // inside an info, otherwise we can just control the content using
      // the messages file
      val tell = "Unit"  // if (tellsRaw.isEmpty) tq"Unit" else tq"String"

      val asks: List[String] = asksRaw.flatMap{ _ match {
        case g: GroupField => g.fields
        case x => List(x)
      }}.flatMap{ f =>
        val inner = f match {
          case f: TextField    => f.format match {
            case Some(s) if s.startsWith("number") => List("Int") // TODO: Properly parse the formats
            case _             => List("String")
          }
          case i: InfoField    => List("Unit")
          case c: ChoiceField if c.isDisguisedBoolean  => List("Boolean")
          case c: ChoiceField if c.multivalue =>
            List(s"Set[${c.id.trim.split(" ").toList.camel}]")
          case c: ChoiceField  => List(c.id.trim.split(" ").toList.camel)
          case d: DateField    => List("LocalDate")
          case f: FileField    => List("File")
          case a: AddressField => List("Address")
          case o               => throw new IllegalArgumentException(s"Don't know how to handle: $o")
        }
        if (f.mandatory) inner else inner.map(x => s"Option[$x]")
      }

      val newConstructs: List[String] = asksRaw.flatMap{ _ match {
        case g: GroupField => g.fields
        case x => List(x)
      }}.flatMap {
        case cf: ChoiceField if !cf.isDisguisedBoolean =>
          dataConstruct(cf)
        case _ => Nil
      }

      val tupled = asks match {
        case Nil => "Unit"
        case solo::Nil => solo
        case xs => "(" + xs.mkString(",") + ")"
      }

      val ask = if (s.isList) s"List[$tupled]" else tupled
      (tell,ask,newConstructs)
    }

    val sectionDataType: Map[Section,(String, String, List[String])] = sections.map{ sec=> 
      sec -> _sectionDataType(sec)
    }.toMap    

    val pinner = sections.map{ sec =>
      val (tellType, askType,_) = sectionDataType(sec)

      if (tellType === "Unit" && askType === "Unit") {
        s"""_ <- ask[$askType]("${id(sec)}").in[R]"""
      } else if (tellType === "Unit") {
        sec.includeIf match {
          case Some(cond) =>

            val condExpr = Expression.parse(cond.tail.tail.init.replaceAll("[()]",""), sections) // TODO: Parens need to be parsed
            s"""${id(sec)} <- ask[$askType]("${id(sec)}").in[R] when ($condExpr)"""
          case None       =>
            s"""${id(sec)} <- ask[$askType]("${id(sec)}").in[R]"""
        }
      } else if (askType === "Unit") {
        s"""_ <- tell[$tellType]("${id(sec)}")("").in[R]"""
      } else {
        s"""${id(sec)} <- dialogue[$tellType,$askType]("${id(sec)}")("").in[R]"""
      }
    }

    val outputFields = sections.flatMap{ sec =>
      val (_, askType,_) = sectionDataType(sec)
      if (askType.toString == s"Unit".toString)
        Nil
      else {
        if (sec.includeIf.isDefined) {
          List(s"${id(sec)}: Option[$askType]")
        } else {
          List(s"${id(sec)}: $askType")
        }
      }
    }

    val newConstructs: List[String] = sections.flatMap{ sec =>
      sectionDataType(sec)._3
    }

    val typeDeclarations = sections.map{ case sec =>
      val (tellType, askType,_) = sectionDataType(sec)
      (tellType, askType)
    }.groupBy(_.toString).values.map(_.head)
      .zipWithIndex.flatMap
    { case ((tellType, askType),i) =>
      val fieldName = s"UniformType$i"
      val fieldNameB = s"_uniformType$i"

      List(
        s"type $fieldName[A] = Uniform[$tellType, $askType,A]",
        s"type $fieldNameB[R] = $fieldName |= R"
      )
    }

    val typeCount: Int = typeDeclarations.size / 2

    val evidence = {1 to typeCount}.map{ c =>
      val i = c - 1
      s"ufevidence${c}: _uniformType$i[R]"
    }

    val elements: List[String] = {0 until typeCount}.toList.map{ i =>
      s"UniformType$i"
    }

    def balancedStack(elements: List[String]): String = {
      val nodes = elements.grouped(3).collect{
        case (l::m::r::Nil) => s"Fx3[$l,$m,$r]"
        case (l::r::Nil) =>    s"Fx2[$l,$r]"
        case (f::Nil) =>       s"Fx1[$f]"
      }.toList

      @annotation.tailrec
      def pairs(in: List[String]): String = in match {
        case sole::Nil => sole
        case xs => pairs(xs.grouped(2).toList.collect{
          case l::r::Nil => s"FxAppend[$l,$r]"
          case l::Nil => l
        })
      }

      pairs(nodes)
    }

    val addressLine = config.addressClass match {
      case Some(fqClass) =>
        val frags = fqClass.split("\\.")
        val packagePrefix = frags.init.toList match {
          case Nil => ""
          case xs   => xs.init.mkString(".") + "."
        }
        s"""import $packagePrefix{${frags.last} => Address}"""
      case None => "case class Address(line1: String, line2: String, line3: String, line4: String, postcode: String)"
    }

    val applyArguments = sections
      .filter{sec => sectionDataType(sec)._2 != "Unit"}
      .map(s => id(s))
      .mkString(", ")

    val journey = s"""
package ${config.journeyPackage}

import org.atnos.eff._
import ltbs.uniform._
import java.time.LocalDate
import java.io.File
import cats.implicits._

object ${template._id} {
  $addressLine

  case class JourneyOutput(
    ${outputFields.mkString(",\n    ")}
  )

  ${typeDeclarations.mkString("\n  ")}

  ${newConstructs.mkString("\n  ")}

  type Stack = ${balancedStack(elements)}

  def program[R: _uniformCore](implicit ${evidence.mkString(", ")}): Eff[R, JourneyOutput] =
    for {
      ${pinner.mkString("\n      ")}
    } yield (JourneyOutput(${applyArguments}))

  val messages = ${messagesToStatement(extractMessages(template,config))}

}"""

    val controller = config.controllerPackage.map{packageName =>

      val dataTypes = sections.map{ case sec =>
        val (tellType, askType,_) = sectionDataType(sec)
        (tellType, askType)
      }.distinct

      val (listTypes,nonListTypes) = dataTypes.partition{ _._2.startsWith("List[")}

      val uberStack: String = balancedStack(
        List("State[UniformCore, ?]", "Either[Result, ?]") ++
          dataTypes.collect {
            case (a,t) => s"Uniform[$a, $t,?]"
          }
      )

      val listedStack: String = balancedStack(
        List("State[UniformCore, ?]", "Either[Result, ?]") ++ 
        dataTypes.flatMap { _ match {
          case ("Unit", ask) if ask.startsWith("List[") =>
            val field = ask.drop(5).dropRight(1)
            List(
              (s"List[$field]","ListControl"),
              (field,"Boolean"),
              ("Unit", field)
            )
          case _ => Nil
        } }.map{
          case (a,t) => s"Uniform[$a, $t,?]"
        }
      )

      val dataParsers = dataTypes.flatMap{ case (t,a) =>
        List(
          s"DataParser[$a]",
          s"HtmlForm[$a]",
          s"($t, String) => Html"
        )
      }.distinct.zipWithIndex.map{ case (v,i) => 
          s"i$i: $v"
      }.mkString(",")

      s"""
package $packageName

import cats.data.State
import java.io.File 
import java.time.LocalDate
import ltbs.uniform._,interpreters.playframework._,web._
import org.atnos.eff._
import play.api.mvc._
import play.twirl.api.Html
import scala.concurrent._

import ${config.journeyPackage}.${template._id}._

trait ${template._id}Controller extends PlayInterpreter {

  type UberStack = $uberStack

  def persistence(in: Request[AnyContent]): Persistence

  type ListedStack = $listedStack

  def interpretedJourney(finalAction: JourneyOutput => Future[Result])(implicit ec: ExecutionContext, ids: List[String], request: Request[AnyContent], msg: Messages, $dataParsers): Future[Result] = {
    def listedJourney: Eff[ListedStack, JourneyOutput] = 
      program[FxAppend[Stack,PlayStack]]
        ${nonListTypes.map{ case (a,t) => 
        s".useForm(PlayForm.automatic[$a, $t])"
        }.mkString("\n        ")}

    runWeb(
      program = listedJourney,
      persistence(request)
    )(
      finalAction
    )
  }
  
}
"""}

    val tests = config.logicTableTests match {
      case true => Some(s"""

""")
      case false => None
    }

    (journey,controller,tests)
  }
}


object Expression {

  def parse(in: String, sections: List[Section]): String = {
    import scala.util.parsing.combinator._

    def findQuestion(key: String): Option[(Section,Field)] = for {
      s <- sections.find(_.fields.exists(_.id === key))
      f <- s.fields.find(_.id === key)
    } yield (s,f)

    def mapPath(key: String): String = {
      findQuestion(key) match {
        case Some((s,_)) => s.id
        case None    => key
      }
    }

    def fieldId(id: String): (String, Option[String]) = {
      findQuestion(id) map { case (section,field) => 
        val secId = section.id
        val nonInfoFields = section.fields.filterNot(_.isInstanceOf[InfoField])
        val projection = nonInfoFields match {
          case `field` :: Nil => None
          case fs => Some("_" ++ {fs.indexOf(field)+1}.toString)
        }
        (secId, projection)
      } getOrElse ((id,None))
    }

    object HorribleParser extends RegexParsers {
      def identifier: Parser[String] = """[a-zA-Z][a-zA-Z0-9]+""".r

      def op: Parser[String]    = """[!=+-/*|&]+""".r ^^ { _ match {
        case "=" => "eqv"
        case "!=" => "neqv"
        case o   => nameEncode(o)
      } }

      def value: Parser[String] = """[0-9]+""".r

      def condition: Parser[String] = identifier ~ op ~ value ^^ {
        case i ~ o ~ v =>
          val path = mapPath(i)
          val value = {
            findQuestion(i) match {
              case Some((_,field: ChoiceField)) if field.isDisguisedBoolean => v.toInt match {
                case 1 => "false"
                case 0 => "true"
              }
              case Some((_,field: ChoiceField)) =>
                field.id.trim.split(" ").toList.camel ++ "." ++
                field.choices.toList(v.toInt).trim.split(" ").toList.camel
              case _ => v
            }
          }

          def isOpt: Boolean = findQuestion(i).map{
            case (sec,ques) => !ques.mandatory || sec.includeIf.isDefined
          }.getOrElse(false)

          val identS = (isOpt,fieldId(i)) match {
            case (false,(sec,Some(proj))) => sec + "." + proj
            case (true,(sec,Some(proj))) =>
              val projFunc = s"_.$proj"
              s"${sec}.map($projFunc)"
            case (_,(sec,None)) => sec
          }

          val valueS = if (isOpt) {
            s"Some($value)"
          } else {
            value
          }

          List(identS, o, valueS).mkString(" ")
      }
    }

    HorribleParser.parse(HorribleParser.condition,in) match {
      case HorribleParser.Success(x,_) => x
      case e => throw new RuntimeException(e.toString())
    }
  }
}
