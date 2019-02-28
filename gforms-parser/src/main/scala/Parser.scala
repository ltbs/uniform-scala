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

  def sectionDataType(c: Context)(s: Section): (c.universe.Tree,c.universe.Tree) = {
    import c.universe._
      val (tellsRaw,asksRaw) = s.fields.partition {
        case i: InfoField    => true
        case _               => false
      }

      // we probably only need to use a tell if we have an expression
      // inside an info, otherwise we can just control the content using
      // the messages file
      val tell = tq"Unit"  // if (tellsRaw.isEmpty) tq"Unit" else tq"String"

      val asks: List[Tree] = asksRaw.flatMap{ _ match {
        case g: GroupField => g.fields
        case x => List(x)
      }}.flatMap{f =>
        val inner = f match {
          case f: TextField    => f.format match {
            case Some(s) if s.startsWith("number") => List(tq"Int") // TODO: Properly parse the formats
            case _             => List(tq"String")
          }
          case i: InfoField    => List(tq"Unit")
          case c: ChoiceField if c.isDisguisedBoolean  => List(tq"Boolean")            
          case c: ChoiceField  => List(tq"String")
          case d: DateField    => List(tq"LocalDate")
          case f: FileField    => List(tq"File")
          case a: AddressField => List(tq"Address")
          case o               => throw new IllegalArgumentException(s"Don't know how to handle: $o")
        }
        if (f.mandatory) inner else inner.map(x => tq"Option[$x]")
      }
      
      val ask = if (s.isList) tq"List[(..$asks)]" else tq"(..$asks)"
      (tell,ask)
    }

  def parse_impl(c: Context)(file: c.Expr[String]): c.universe.Tree = {
    import c.universe._

    def fieldToType(f: Field): c.universe.Tree = {
      val inner = f match {
        case f: TextField    => f.format match {
          case _             => tq"String" }
        case i: InfoField    => tq"Unit"
        case c: ChoiceField  => tq"String"
        case d: DateField    => tq"LocalDate"
        case f: FileField    => tq"File"
        case a: AddressField => tq"Address"
        case o               => throw new IllegalArgumentException(s"Don't know how to handle: $o")
      }
      if (f.mandatory) inner else (tq"Option[$inner]")
    }

    val Literal(Constant(fileInner: String)) = file.tree
    val template = GformDecoder(fileInner) match {
      case Right(x) => x
      case Left(l) => throw new Exception(l.toString)
    }

    val sections: List[Section] = template.allSections

    def id(sec: Section) = template.deduplicatedSectionId(sec)

    val pinner = sections.map{ sec => 
      val (tellType, askType) = sectionDataType(c)(sec)

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
      val (_, askType) = sectionDataType(c)(sec)
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

    val enMessages: Map[String, String] = Map.empty// fields.map{ f =>
    //   s"${f.id}.label" -> f.label
    // }.toMap

    val typeDeclarations = sections.map{ case sec =>
      val (tellType, askType) = sectionDataType(c)(sec)
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



    val r = q"""new {
  import org.atnos.eff._
  import ltbs.uniform._
  import ltbs.uniform.gformsparser._
  import java.time.LocalDate
  import java.io.File
  import cats.implicits._

  case class JourneyOutput(..$outputFields)

  ..$typeDeclarations

  //type Stack = Fx.fx6[UniformAskString,UniformAskDate,UniformAskFile,UniformAskUnit,UniformAskAddress,UniformAskOptionString]

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
              case Some((section,field: ChoiceField)) if field.isDisguisedBoolean => v.toInt match {
                case 1 => Constant(false)
                case 0 => Constant(true)
              }
              case _ => Constant(v.toInt)
            }
          }
          Apply(Select(Ident(TermName(path)), o), List(Literal(value)))
      }
    }

    HorribleParser.parse(HorribleParser.condition,in) match {
      case HorribleParser.Success(x,_) =>
        println(showRaw(x))
        x
      case e => throw new RuntimeException(e.toString())
    }
  }

  def gformExpr_impl(c: Context)(expr: c.Expr[String]): c.universe.Tree = {
    import c.universe._
    val Literal(Constant(expra: String)) = expr.tree
    parseGform(c)(expra, Nil)
  }

}
