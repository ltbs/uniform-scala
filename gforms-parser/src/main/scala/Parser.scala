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

  def parse_impl(c: Context)(file: c.Expr[String]): c.universe.Tree = {
    import c.universe._

    // def sectionDataType(s: Section): Option[Tree] = {
    //   val nonInfoFields = s.fields.filter {
    //     case i: InfoField => false
    //     case _ => true
    //   }
    //   s.fields.filter match {
    //   case
    // }

    def fieldToType(f: Field): Tree = {
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
    val template = GformDecoder(fileInner)
    val fields: List[Field] = template match {
      case Right(x) => x.allSections.flatMap{_.fields}.flattenGroups
      case Left(l) => throw new Exception(l.toString)
    }

    val pinner = fields.map{
        case f if fieldToType(f) == TypeName("Unit") => fq"""_ <- ask[Unit](${f.id}).in[R] """
//        case c: ChoiceField      => fq"""${TermName(nameEncode(c.id))} <- uaskOneOf[R, ${fieldToType(c)}](${c.id}, ${c.choices})"""
        case other               => fq"""${TermName(nameEncode(other.id))} <- ask[${fieldToType(other)}](${other.id}).in[R]"""
    }

    fields.flatMap(_.validIf).map(x => throw new Exception(x))

    val enMessages: Map[String, String] = fields.map{ f =>
      s"${f.id}.label" -> f.label
    }.toMap

    val r = q"""
new {
  import org.atnos.eff._
  import ltbs.uniform._
  import ltbs.uniform.gformsparser._
  import java.time.LocalDate
  import java.io.File

  type UniformAskString[A] = UniformAsk[String, A]
  type UniformAskOptionString[A] = UniformAsk[Option[String], A]
//  type UniformChooseString[A] = UniformSelect[String, A]
  type UniformAskDate[A] = UniformAsk[java.time.LocalDate, A]
  type UniformAskFile[A] = UniformAsk[java.io.File, A]
  type UniformAskUnit[A] = UniformAsk[Unit, A]
  type UniformAskAddress[A] = UniformAsk[Address, A]

  type Stack = Fx.fx6[UniformAskString,UniformAskDate,UniformAskFile,UniformAskUnit,UniformAskAddress,UniformAskOptionString]
  type _ufString[R] = UniformAskString |= R
  type _ufOptString[R] = UniformAskOptionString |= R
  type _ufDate[R] = UniformAskDate |= R
  type _ufFile[R] = UniformAskFile |= R
  type _ufUnit[R] = UniformAskUnit |= R
  type _ufAddress[R] = UniformAskAddress |= R
//  type _ufChoose[R] = UniformChooseString |= R
  def program[R : _uniformCore : _ufString : _ufDate : _ufFile : _ufUnit : _ufAddress : _ufOptString]: Eff[R, Unit] =
    for (..$pinner) yield (())

  val messages = Map("default" -> $enMessages)
}"""
//    println(r)
    r
  }

  def gformExpr(expr: String): Any = macro gformExpr_impl

  def parseGform(c: Context)(in: String, fields: List[Field]): c.universe.Tree = {
    import c.universe._
    import scala.util.parsing.combinator._

    object HorribleParser extends RegexParsers {
      def identifier: Parser[TermName] = """[a-zA-Z][a-zA-Z0-9]+""".r ^^ { TermName(_) }
      def op: Parser[TermName]    = """[=+-/*|&]+""".r ^^ { _ match {
                                                             case "=" => TermName(nameEncode("=="))
                                                             case o   => TermName(nameEncode(o))
                                                           }
      }
      def value: Parser[Literal] = """[0-9]+""".r ^^ { x => Literal(Constant(x.toInt)) }
      def condition: Parser[Tree] = identifier ~ op ~ value ^^ {
        case i ~ o ~ v => Apply(Select(Ident(i), o), List(v))}
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
