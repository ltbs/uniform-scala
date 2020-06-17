package ltbs.uniform

import cats.instances.either._
import cats.instances.list._
import cats.syntax.alternative._
import com.github.ghik.silencer.silent
import scala.language.higherKinds
import scala.reflect.macros.blackbox
import shapeless.HList
import scala.annotation.tailrec

class TypeclassListMacros(val c: blackbox.Context) {
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

  /** 
    * Turn a Needs[_] type into a (List[AskTypes], List[TellTypes]) 
    */ 
  @silent("never used") // quasiquoting seems to produce lots of false warnings  
  def getNeeds[H <: Needs[_]](
    implicit ttn: c.WeakTypeTag[H]
  ): (List[Tree], List[Tree]) = {
    val code = s"type X = ${ttn.tpe.toString}"
    val xxx = c.parse(code)
    val TypeDef(_, _, _, ctt) = xxx
    val parents = ctt match {
      case tq"..$p { }" => p
      case x => List(x)
    }
    val ret = parents.collect{
      case tq"ltbs.uniform.Needs.Ask[$x]" => Left(x)
      case tq"ltbs.uniform.Needs.Tell[$x]" => Right(x)        
    }
    ret.separate
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

  def fromImplicits2(
    fType: Type,
    elems: List[Tree]
  ): Tree = {

    val z = fType.typeSymbol
    val kelems = elems.map(x => tq"$z[${x}]")
    val hl = hlist(elems)
    val list = implicitKList(fType, elems)
     val r = q"""
import shapeless._
new TypeclassList[$hl, ${fType.typeSymbol}] {
  type Repr = ${hlist(kelems)}
  val list: Repr = $list
}"""
    r
  }

  def implicitMaps(
    fType: Type, 
    types: List[Tree]
  ): Tree = {

    val symbol = fType.typeSymbol

    val typeArgs: List[Type] = fType.typeArgs.takeWhile(_.toString != "Any")

    @tailrec
    def inner(types: List[Tree], acc: Tree): Tree = types match {
      case v::x => inner(x, q"(implicitly[izumi.reflect.Tag[$v]].tag, implicitly[${symbol}[..$typeArgs, $v]]) :: $acc")
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
    val (askTypes, tellTypes) = getNeeds
    val askMap = implicitMaps(ttAskTc.tpe, askTypes)
    c.info(c.enclosingPosition, askMap.toString, true)

    val tellMap = implicitMaps(ttTellTc.tpe, tellTypes)    

    val r = q"${c.prefix}.executeImpl($program, $askMap, $tellMap)"
    
//    val r = reify{ clazz.splice.executeImpl(program.splice, askList.splice, tellList.splice) }
//    println(r)
    c.Expr[F[A]](r)
  }


}
