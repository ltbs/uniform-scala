package ltbs.uniform.gformsparser

import scala.language.experimental.macros
//import scala.reflect.macros.Context

import scala.reflect.macros.whitebox.Context
import scala.collection.mutable.{ListBuffer, Stack}
import ltbs.uniform._
import java.io.File
import org.atnos.eff._
import scala.util._
import cats.implicits._
import scala.reflect.NameTransformer.{ encode => nameEncode }

object Parser {

  def parseGform(file: String): AnyRef = macro parse_impl

  def dataConstruct(c: Context)(cf: ChoiceField): List[c.universe.Tree] = {
    import c.universe._

    val nameString = cf.id.trim.split(" ").toList.camel
    val name = TypeName(nameString)
    val opts : List[Tree] = cf.choices.map { option =>
      val optionIdent = TermName(option.trim.split(" ").toList.camel)
      q"case object $optionIdent extends $name"
    }.toList 
    List(
      q"sealed trait $name",
      q"object ${TermName(nameString)} { ..$opts }",
      q"""implicit val ${TermName("eq" ++ nameString)} = cats.kernel.Eq.fromUniversalEquals[${TypeName(nameString)}]"""
    )
  }

  def sectionDataType(c: Context)(s: Section): (c.universe.Tree, c.universe.Tree, List[c.universe.Tree]) = {
    import c.universe._
    val (tellsRaw,asksRaw) = s.fields.partition { _.isInstanceOf[InfoField]}

    // we probably only need to use a tell if we have an expression
    // inside an info, otherwise we can just control the content using
    // the messages file
    val tell = tq"Unit"  // if (tellsRaw.isEmpty) tq"Unit" else tq"String"

    val asks: List[Tree] = asksRaw.flatMap{ _ match {
      case g: GroupField => g.fields
      case x => List(x)
    }}.flatMap{ f =>
      val inner = f match {
        case f: TextField    => f.format match {
          case Some(s) if s.startsWith("number") => List(tq"Int") // TODO: Properly parse the formats
          case _             => List(tq"String")
        }
        case i: InfoField    => List(tq"Unit")
        case c: ChoiceField if c.isDisguisedBoolean  => List(tq"Boolean")
        case c: ChoiceField if c.multivalue =>
          List(AppliedTypeTree(Ident(TypeName("Set")), List(Ident(TypeName(c.id.trim.split(" ").toList.camel)))))
        case c: ChoiceField  => List(Ident(TypeName(c.id.trim.split(" ").toList.camel)))
        case d: DateField    => List(tq"LocalDate")
        case f: FileField    => List(tq"File")
        case a: AddressField => List(tq"Address")
        case o               => throw new IllegalArgumentException(s"Don't know how to handle: $o")
      }
      if (f.mandatory) inner else inner.map(x => tq"Option[$x]")
    }

    val newConstructs: List[Tree] = asksRaw.flatMap{ _ match {
      case g: GroupField => g.fields
      case x => List(x)
    }}.flatMap {
      case cf: ChoiceField if !cf.isDisguisedBoolean =>
        dataConstruct(c)(cf)
      case _ => Nil
    }

    val ask = if (s.isList) tq"List[(..$asks)]" else tq"(..$asks)"
    (tell,ask,newConstructs)
  }

  def parse_impl(c: Context)(file: c.Expr[String]): c.universe.Tree = {
    import c.universe._

    val Literal(Constant(fileInner: String)) = file.tree
    val template = GformDecoder(fileInner) match {
      case Right(x) => x
      case Left(l) => throw new Exception(l.toString)
    }

    val sections: List[Section] = template.allSections

    def id(sec: Section) = template.deduplicatedSectionId(sec)

    val pinner = sections.map{ sec => 
      val (tellType, askType,_) = sectionDataType(c)(sec)

      if (tellType.toString == tq"Unit".toString && askType.toString == tq"Unit".toString) {
        fq"""_ <- ask[$askType](${id(sec)}).in[R]"""
      } else if (tellType.toString == tq"Unit".toString) {
        sec.includeIf match {
          case Some(cond) =>

            val condExpr = parseGform(c)(cond.tail.tail.init.replaceAll("[()]",""), sections) // TODO: Parens need to be parsed 
            fq"""${TermName(id(sec))} <- ask[$askType](${id(sec)}).in[R] when ($condExpr)"""
          case None       => 
            fq"""${TermName(id(sec))} <- ask[$askType](${id(sec)}).in[R]"""
        }
      } else if (askType.toString == tq"Unit".toString) {
        fq"""_ <- tell[$tellType](${id(sec)})("").in[R]"""
      } else {
        fq"""${TermName(id(sec))} <- dialogue[$tellType,$askType](${id(sec)})("").in[R]"""
      }
    }

    val outputFields = sections.flatMap{ sec => 
      val (_, askType,_) = sectionDataType(c)(sec)
      if (askType.toString == tq"Unit".toString)
        Nil
      else {
        if (sec.includeIf.isDefined) {
          List(q"${TermName(id(sec))}: Option[$askType]")
        } else {
          List(q"${TermName(id(sec))}: $askType")
        }
      }
    }

    val newConstructs: List[Tree] = sections.flatMap{ sec =>
      sectionDataType(c)(sec)._3
    }

    val enMessages: Map[String, String] = 
      template.sections.flatMap { case section =>
        val sectionId = template.deduplicatedSectionId(section)
        (s"${sectionId}.title" -> section.title) ::
        section.infoFields.zipWithIndex.map {
          case (i,index) => (s"${sectionId}.info${index+1}" -> i.infoText)
        } ++
        section.fields.flatMap { field => 
          val helpText = field.helpText.map{t => (s"${sectionId}.${field.id}.hint" -> t)}
          val label = Some(field.label).filter(_.trim.nonEmpty).map{t => (s"${sectionId}.${field.id}.label" -> t)}
          List(helpText,label).flatten
        }
      }.toMap

    val typeDeclarations = sections.map{ case sec =>
      val (tellType, askType,_) = sectionDataType(c)(sec)
      (tellType, askType)
    }.groupBy(_.toString).values.map(_.head)
      .zipWithIndex.flatMap
    { case ((tellType, askType),i) =>
      val fieldName = TypeName(s"UniformType$i")
      val fieldNameB = TypeName(s"_uniformType$i")
      
      List(
        q"type $fieldName[A] = Uniform[$tellType, $askType,A]",
        q"type $fieldNameB[R] = $fieldName |= R"
      )
    }

    val typeCount: Int = typeDeclarations.size / 2

    val evidence = {1 to typeCount}.map{ c =>
      val i = c - 1
      q"${TermName(s"ufevidence$$${c}")}: ${TypeName(s"_uniformType$i")}[R]"
    }

    val stack = {
      val (headOfStack::tailOfStack) = {0 until typeCount}.toList.map{ i =>
        TypeName(s"UniformType$i")
      }

      tailOfStack.foldLeft(tq"Fx.fx1[$headOfStack]"){
        case (acc,e) => tq"Fx.prepend[$e,$acc]"
      }
    }

    val r = q"""new {
  import org.atnos.eff._
  import ltbs.uniform._
  import ltbs.uniform.gformsparser._
  import java.time.LocalDate
  import java.io.File
  import cats.implicits._

  case class JourneyOutput(..$outputFields)

  ..$typeDeclarations

  ..$newConstructs

  type Stack = $stack

  def program[R: _uniformCore](implicit ..$evidence): Eff[R, Unit] =
    for (..$pinner) yield (JourneyOutput(..${sections.map(s => TermName(id(s)))}))

  val messages = Map("default" -> $enMessages)
}"""
    println(r)
    r
  }

  def gformExpr(expr: String): Any = macro gformExpr_impl

  def parseGform(c: Context)(in: String, sections: List[Section]): c.universe.Tree = {
    import c.universe._
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

      def op: Parser[TermName]    = """[!=+-/*|&]+""".r ^^ { _ match {
        case "=" => TermName(nameEncode("eqv"))
        case "!=" => TermName(nameEncode("neqv"))
        case o   => TermName(nameEncode(o))
      } }

      def value: Parser[String] = """[0-9]+""".r

      def condition: Parser[Tree] = identifier ~ op ~ value ^^ {
        case i ~ o ~ v =>
          val path = mapPath(i)
          val value = {
            findQuestion(i) match {
              case Some((_,field: ChoiceField)) if field.isDisguisedBoolean => v.toInt match {
                case 1 => Literal(Constant(false))
                case 0 => Literal(Constant(true))
              }
              case Some((_,field: ChoiceField)) =>
                Select(Ident(
                  TermName(field.id.trim.split(" ").toList.camel)),
                  TermName(field.choices.toList(v.toInt).trim.split(" ").toList.camel)
                )
              case _ => Literal(Constant(v.toInt))
            }
          }

          def isOpt: Boolean = findQuestion(i).map{
            case (sec,ques) => !ques.mandatory || sec.includeIf.isDefined
          }.getOrElse(false)

          val identS = (isOpt,fieldId(i)) match {
            case (false,(sec,Some(proj))) => Select(Ident(TermName(sec)), TermName(proj))
            case (true,(sec,Some(proj))) =>
              val projIdent: TermName = TermName(proj)
              val projFunc: Function = q"_.$projIdent"
              q"${TermName(sec)}.map($projFunc)"
            case (_,(sec,None)) => Ident(TermName(sec))
          }

          val valueS = if (isOpt) {
            List(Apply(Ident(TermName("Some")), List(value)))
          } else {
            List(value)
          }

          Apply(Select(identS, o), valueS)
      }
    }

    HorribleParser.parse(HorribleParser.condition,in) match {
      case HorribleParser.Success(x,_) => x
      case e => throw new RuntimeException(e.toString())
    }
  }

  def gformExpr_impl(c: Context)(expr: c.Expr[String]): c.universe.Tree = {
    import c.universe._
    val Literal(Constant(expra: String)) = expr.tree
    parseGform(c)(expra, Nil)
  }

}
