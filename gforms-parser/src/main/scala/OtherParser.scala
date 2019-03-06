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
import scala.annotation.StaticAnnotation
import scala.annotation.compileTimeOnly

@compileTimeOnly("enable macro paradise to perform gForm conversions")
class gform extends StaticAnnotation {
  def macroTransform(annottees: Any*): Any = macro gformMacro.impl
}

object gformMacro {

  def dataConstruct(c: Context)(cf: ChoiceField): List[c.universe.Tree] = {
    import c.universe._

    val nameString = cf.id.trim.split(" ").toList.camel
    val name = TypeName(nameString)
    val opts : List[Tree] = cf.choices.map { option =>
      val optionIdent = TermName(option.trim.split(" ").toList.camel)
      q"case object $optionIdent extends $name"
    }.toList

    List(
      q"sealed trait $name extends EnumEntry",
      q"object ${TermName(nameString)} extends Enum[$name] { val values = findValues; ..$opts }",
      q"""implicit val ${TermName("eq" ++ nameString)} = cats.kernel.Eq.fromUniversalEquals[${TypeName(nameString)}]"""
    )
  }

  def implicitParser(c: Context)(cf: ChoiceField): c.universe.Tree = {
    import c.universe._
    val nameString = cf.id.trim.split(" ").toList.camel
    q"""lazy implicit val ${TermName(cf.id.trim.split(" ").toList.lowerCamel + "Parser")} = implicitly[DataParser[${TypeName(nameString)}]]"""
  }

  def sectionDataType(c: Context)(s: Section): (c.universe.Tree, c.universe.Tree, List[c.universe.Tree], List[c.universe.Tree]) = {
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

    val implicitParsers: List[Tree] = asksRaw.flatMap{ _ match {
      case g: GroupField => g.fields
      case x => List(x)
    }} collect {
      case cf: ChoiceField if !cf.isDisguisedBoolean =>
        implicitParser(c)(cf)
    }

    val ask = if (s.isList) tq"List[(..$asks)]" else tq"(..$asks)"
    (tell,ask,newConstructs,implicitParsers)
  }


  def impl(c: Context)(annottees: c.Expr[Any]*): c.Expr[Any] = {
    import c.universe._
    val outputs = annottees.toList.map { classDecl =>

      val q"object $className extends ..$bases { ..$body }" = classDecl.tree
      val TermName(classNameString) = className
      val fileName = s"gforms/$classNameString.json"
      if (!{new java.io.File(fileName).exists}) c.abort(c.enclosingPosition, s"Unable to read $fileName")

      val template = GformDecoder(fileName) match {
        case Right(x) => x
        case Left(l) => c.abort(c.enclosingPosition, s"Unable to parse $fileName: $l")
      }

      val sections: List[Section] = template.allSections

      def id(sec: Section) = template.deduplicatedSectionId(sec)

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

      val pinner = sections.map{ sec =>
        val (tellType, askType,_,_) = sectionDataType(c)(sec)

        if (tellType.toString == tq"Unit".toString && askType.toString == tq"Unit".toString) {
          fq"""_ <- ask[$askType](${id(sec)}).in[R]"""
        } else if (tellType.toString == tq"Unit".toString) {
          sec.includeIf match {
            case Some(cond) =>
              val condExpr = Parser.parseGform(c)(cond.tail.tail.init.replaceAll("[()]",""), sections) // TODO: Parens need to be parsed
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
        val (_, askType,_,_) = sectionDataType(c)(sec)
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

    val typeDeclarations = sections.map{ case sec =>
      val (tellType, askType,_,_) = sectionDataType(c)(sec)
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
      q"${TermName(s"ufevidence${c}")}: ${TypeName(s"_uniformType$i")}[R]"
    }

    val stack = {
      val elements = {0 until typeCount}.toList.map{ i =>
        TypeName(s"UniformType$i")
      }

      val (headGroup::tailGroup) = elements.grouped(3).map{
        case (l::m::r::Nil) => tq"Fx3[$l,$m,$r]"
        case (l::r::Nil) =>    tq"Fx2[$l,$r]"
        case (f::Nil) =>       tq"Fx1[$f]"
      }.toList.reverse

      tailGroup.foldLeft(headGroup){
        case (acc,e) => tq"FxAppend[$e,$acc]"
      }
    }


      q"""
object $className extends ..$bases { 
  import org.atnos.eff._
  import ltbs.uniform._
  import ltbs.uniform.gformsparser._
  import java.time.LocalDate
  case class File(filename: String) 
  import cats.implicits._
  import enumeratum._

  case class JourneyOutput(..$outputFields)

  ..$typeDeclarations

  ..$newConstructs

  type Stack = $stack

  def program[R: _uniformCore](implicit ..$evidence): Eff[R, Unit] =
    for (..$pinner) yield (JourneyOutput(..${sections.map(s => TermName(id(s)))}))

  val messages = Map("default" -> $enMessages)

  ..$body 
}
"""
    }

    c.Expr[Any](Block(outputs, Literal(Constant(()))))
  }
}
