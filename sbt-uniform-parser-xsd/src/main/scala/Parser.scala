package ltbs.uniform.parsers.xsd

import cats.implicits._
import scala.xml._
import java.io.File
import scala.util.Try

object Parser {

  def getClassesFromFile(input: File): Seq[CaseClass] = {
    val complexTypes = XML.loadFile(input) \ "complexType"
    complexTypes.map(decodeComplexType)
  }

  def elements(datafiles: Iterable[Elem]): List[(String,List[Node])] = (datafiles.toList >>= {x => (x \ "element").toList}) map { x => (x \@ "name", (x \ "complexType").toList) }
  def complexTypes(datafiles: Iterable[Elem]): List[(String,Node)] = (datafiles.toList >>= {x => (x \ "complexType").toList}) map { x => (x \@ "name", x) }

  implicit class RichList[A](in: List[A]) {
    def partitionWhile(pred: A => Boolean): (List[A], List[A]) = {
      @annotation.tailrec
      def inner(yes: List[A], no: List[A]): (List[A], List[A]) = no match {
        case Nil => (yes.reverse,no)
        case (nh::nt) if pred(nh) => inner(nh::yes, nt)
        case (nh::nt) => (yes.reverse,no)
      }
      inner(Nil, in)
    }
  }

  case class Field(name: String, dataType: String, min: Int, max: Int) {

    def niceName = name match {
      case "type" | "Type" => "`type`"
      case _ =>
        val (a,b) = name.toList.partitionWhile(_.isUpper)
        a.map(_.toLower).mkString ++ b.mkString
    }

    def scalaTypeNoCardinality = dataType match {
      case "xs:string" => "String"
      case "xs:decimal" => "Double"
      case "xs:int" | "xs:integer" => "Int"
      case "xs:short" => "Short"
      case "xs:dateTime" => "LocalDateTime"
      case "xs:date" => "LocalDate"
      case "YesNoType" | "xs:boolean" => "Boolean"
      case x if x.endsWith("Type") => x.dropRight(4)
      case x => x
    }

    def scalaType = (min,max) match {
      case (1,1) => scalaTypeNoCardinality
      case (0,1) => "Option[" ++ scalaTypeNoCardinality ++ "]"
      case _     => "List[" ++ scalaTypeNoCardinality ++ "]"
    }

    override def toString: String = s"$niceName: $scalaType"

    def uniformFragment: String = {
      val ask = scalaType match {
        case "String" | "Double" | "Int" | "LocalDate" | "LocalDateTime" | "Boolean" =>
          s"""uask[S,${dataType}]("${niceName}")"""
        case o => s"""${dataType}.ask("${niceName}")"""
      }
      s"""  ${niceName} <- $ask"""
    }
  }

  case class CaseClass(name: String, fields: List[Field]) {

    def niceName = name match {
      case x if x.endsWith("Type") => x.dropRight(4)
      case x => x
    }

    override def toString: String = s"""case class $niceName (\n  """ ++
      fields.map(_.toString).mkString(",\n  ") ++ "\n)"

    def companion: String = s"object $niceName { $uniformFragment }"

    def uniformFragment: String = s"""def ask(id: String) = (\n  """ ++
      fields.map(_.uniformFragment).mkString(",\n  ") ++ "\n)".mkString(",\n") ++
      s"\n).mapN(${niceName}.apply)"

  }

  def decodeComplexType(in: Node): CaseClass = {

    def tryNumber(in: String): Int = Try(in.toInt).getOrElse(1)

    def decodeElement(in: Node): Field = {
      Field(in \@ "name", (in \@ "type"), tryNumber(in \@ "minOccurs"), tryNumber(in \@ "maxOccurs"))
    }

    val name = in \@ "name"
    val elements = (in \ "sequence" \ "element").toList.map(decodeElement)
    CaseClass(name, elements)
  }
}
