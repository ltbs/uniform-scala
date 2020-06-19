package ltbs.uniform

import cats.instances.either._
import cats.instances.list._
import cats.syntax.alternative._
import com.github.ghik.silencer.silent
import scala.language.higherKinds
import scala.reflect.macros.whitebox
import shapeless.HList
import scala.annotation.tailrec

class TypeclassListMacros(val c: whitebox.Context) {
  import c.universe._

  /**
    * Create a KList value from implicits 
    */
  def implicitKList(f: Type, in: List[Tree]): Tree =
      in.foldRight(Select(Ident(TermName("shapeless")), TermName("HNil")): Tree){
        case (tpe: Tree, expr) =>
          val z = f.typeSymbol
          q"shapeless.::(implicitly[$z[$tpe]],$expr)"
      }

  /**
    * Create a List from a list of elements 
    */
  def hlist(in: List[Tree]): Expr[HList] =
    c.Expr[HList] (
      in.foldRight(tq"shapeless.HNil": Tree){
        case (tpe: Tree, expr) =>
          tq"shapeless.::[$tpe,$expr]"
      }
    )

  def hlistToListOfElements(
    tpe: Type
  ): List[Tree] = {
    val code = s"type X = ${tpe.dealias.toString}"
    val xxx = c.parse(code)
    val TypeDef(_, _, _, ctt) = xxx    

    @tailrec
    def inner(found: List[Tree], remaining: Tree): List[Tree] = {
//      c.info(c.enclosingPosition, "REMAINING: " + remaining.toString, true)
      remaining match {
        case AppliedTypeTree(Ident(TypeName("$colon$colon")), List(typeIdent, n)) =>
          inner(typeIdent :: found, n)
        case Select(Ident(TermName("shapeless")), TypeName("HNil")) => 
          found
        case Select(Select(Ident(TermName("ltbs")), TermName("uniform")), TypeName("NilTypes")) =>
          tq"Unit" :: found
        case x =>
          c.error(c.enclosingPosition, s"expecting a type in a HList, found '$x'")
          found 
      }
    }

    inner(Nil, ctt)
  }

  def fromImplicits[L <: HList, TC[_]](
    hlistTypeTag: c.WeakTypeTag[L],
    fTypeTag: c.WeakTypeTag[TC[_]]
  ): Tree = {
    val elems = hlistToListOfElements(hlistTypeTag.tpe).reverse

    val z = fTypeTag.tpe.typeSymbol
    val kelems = elems.map(x => tq"$z[${x}]")
    val list = implicitKList(fTypeTag.tpe, elems)
     val r = q"""
import shapeless._
new TypeclassList[${hlistTypeTag.tpe}, ${fTypeTag.tpe}] {
  type Repr = ${hlist(kelems)}
  val list: Repr = $list
}"""
    r
  }
  /*
  def implicitMapsFuct(
    fType: Type, 
    types: List[Tree]
  ): Tree = {

    val symbol = fType.typeSymbol
//    val p = symbol.asType.typeParams
    val p = fType.typeParams    
    // c.info(c.enclosingPosition, s"p for ${fType}: ${p}", true)        
    // c.info(c.enclosingPosition, s"symbol for ${fType}: ${fType.typeSymbol}", true)    
    // c.info(c.enclosingPosition, s"typeargs for ${fType}: ${fType.typeArgs}", true)        
    val typeArgsPrefix: List[Type] = fType.typeArgs.takeWhile(_.toString != "Any")
//    c.info(c.enclosingPosition, s"typeargsprefix: $typeArgsPrefix", true)    
    val typeArgsPostfix: List[Type] = fType.typeArgs.dropWhile(_.toString != "Any").drop(1)
//    c.info(c.enclosingPosition, s"typeargspostfix: $typeArgsPostfix", true)        

    @tailrec
    def inner(types: List[Tree], acc: Tree): Tree = types match {
      case v::x =>

        val implicitType: Type = fType match {
          case TypeRef(a, b, d) =>
//            c.info(c.enclosingPosition, s"TypeRef($a, $b, $d)", true)
            //            val example = AppliedTypeTree(Ident(TypeName("Either")), List(Ident(TypeName("String")), Ident(TypeName("A"))))
//            val example2 = AppliedTypeTree(Select(Select(Ident(TermName("blah")), TermName("bleaugh")), TypeName("Either")), List(Ident(TypeName("String")), Ident(TypeName("A"))))
            val constructedType = tq"$a.${b.asType}[..$d]"
//            c.info(c.enclosingPosition, s"gives '${constructedType}' (${showRaw(constructedType)})", true)
            c.typecheck(constructedType).tpe
          case _ => ???
        }

        // val implicitType: Type = Option(tq"$fType[$v]".tpe) match {
        //   case None =>
        //     c.abort(c.enclosingPosition, s"Cannot construct type for $fType with $v applied")
        //   case Some(x) => 
        //     c.info(c.enclosingPosition, s"Type for $fType with $v applied is $x", true)
        //     x
        // }

        val lookupP = c.inferImplicitValue(implicitType)
        val lookup = lookupP match {
          case EmptyTree => c.abort(c.enclosingPosition, s"Cannot find an implicit ${implicitType}")
          case x => x
        }
        val append = q"(implicitly[izumi.reflect.Tag[$v]].tag, $lookup) :: $acc"
        inner(x, append)
      case Nil => acc
    }

    val s = inner(types, q"Nil")
    q"Map($s :_*)"
  }
   
  def interpreter_impl[H <: Needs[_], A, ASKTC[_], TELLTC[_], F[_], T](
    program: Expr[Uniform[H,A,T]]
  )(
    implicit ttn: WeakTypeTag[H], 
    ttAskTc: WeakTypeTag[ASKTC[_]],
    ttTellTc: WeakTypeTag[TELLTC[_]]
  ): c.Expr[F[A]] = {

    c.info(c.enclosingPosition, s"H: ${ttn.tpe} / " + showRaw(ttn.tpe), true)
    
    val (askTypes, tellTypes) = getNeeds
    c.info(c.enclosingPosition, s"ASKTC: ${ttAskTc.tpe} /" + showRaw(ttAskTc.tpe), true)
    c.info(c.enclosingPosition, s"TELLTC: ${ttTellTc.tpe} / " + showRaw(ttTellTc.tpe), true)        

//    c.info(c.enclosingPosition, "ASKTYPES: " ++ askTypes.map{_.toString}.mkString(", "), true)
//    c.info(c.enclosingPosition, "TELLTYPES: " ++ tellTypes.map{_.toString}.mkString(", "), true)        
//    c.info(c.enclosingPosition, ttAskTc.tpe.typeParams.toString(), true)
    val askMap = implicitMaps(ttAskTc.tpe, askTypes)
//    c.info(c.enclosingPosition, askMap.toString, true)

    val tellMap = implicitMaps(ttTellTc.tpe, tellTypes)    

    val r = q"${c.prefix}.executeImpl($program, $askMap, $tellMap)"
    
//    val r = reify{ clazz.splice.executeImpl(program.splice, askList.splice, tellList.splice) }
//    println(r)
    c.Expr[F[A]](r)
  }
   */

}
