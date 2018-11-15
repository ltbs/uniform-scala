package ltbs.uniform.gformsparser

import scala.reflect.macros.{ Universe }
import scala.reflect.macros.whitebox.Context

class GformExpressionParser(val c: Context) {

  import c.universe._

  sealed trait Expression {
    def out: c.Tree
  }
  case class GformExpression(out: c.Tree) extends Expression
  case class ScalaExpression(out: c.Tree) extends Expression

  def parseGform(in: String, fields: List[Field]): c.Tree = {
    import scala.util.parsing.combinator._

    object HorribleParser extends RegexParsers {
      def identifier: Parser[TermName] = """[a-zA-Z][a-zA-Z0-9]+""".r ^^ { TermName(_) }
      def op: Parser[TermName]    = """[=+-/*|&]+""".r ^^ { _ match {
          case "=" => TermName("==")
          case o   => TermName(o)
        }
      }
      def value: Parser[Literal] = """[0-9]+""".r ^^ { x => Literal(Constant(x.toInt)) }
      def condition: Parser[Tree] = identifier ~ op ~ value ^^ {
        case i ~ o ~ v => Apply(Select(Ident(i), o), List(v))}

      def apply(in: String): c.Tree = parse(condition,in) match {
        case Success(x,_) =>
          throw new RuntimeException(showRaw(x))
          x
        case e => throw new RuntimeException(e.toString())
      }
    }

    HorribleParser(in)
  }

  def parseNative(in: String): c.Tree = c.parse(in)

  def apply(raw: String, fields: List[Field]): Option[Expression] = raw.trim match {
    case x if x.startsWith("${") => Some(GformExpression(parseGform(x, fields)))
    case x if x.startsWith("#{") => Some(ScalaExpression(parseNative(x)))
    case x => None
  }

}
