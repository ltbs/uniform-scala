package ltbs.uniform

import scala.language.higherKinds
import scala.reflect.macros.blackbox
import scala.annotation.tailrec

class InterpreterMacros(val c: blackbox.Context) {
  import c.universe._

  /**
    * Turn a Needs[_] type into a (List[InteractTypes], List[(ConvFrom, ConvTo)], List[AskListType]) 
    */ 
  def getNeeds[H <: Needs[_, _], INTERACTTC[_,_], ASKLISTTC[_]](
    implicit ttn: c.WeakTypeTag[H], 
    ttInteractTc: WeakTypeTag[INTERACTTC[_,_]],
    ttAskListTc: WeakTypeTag[ASKLISTTC[_]]
  ): (List[(Type, Type)], List[(Type, Type)], List[Type]) = {

    val INTERACT: TypeSymbol = symbolOf[Needs.Interact[_,_]]
    val CONV = symbolOf[Needs.Convert[Any,_]]
    val ASKLIST = symbolOf[Needs.AskList[_]]        

    ttn.tpe match {
      case _: ExistentialType => (Nil, Nil, Nil)
      case TypeRef(_, INTERACT, List(inta, intb)) => ((inta,intb) :: Nil, Nil, Nil)
      case TypeRef(_, CONV, List(convf, conva)) => (Nil, (convf, conva) :: Nil, Nil)
      case TypeRef(_, ASKLIST, List(askl)) => (Nil, Nil, askl :: Nil)                
      case RefinedType(parents,_) =>
        parents.foldLeft((List.empty[(Type,Type)], List.empty[(Type, Type)], List.empty[Type])){
          case ((iacc, convacc, alacc), x) =>
            x match {
              case TypeRef(_, INTERACT, List(inta, intb)) => ((inta,intb) :: iacc, convacc, alacc)
              case TypeRef(_, CONV, List(convf, conva)) => (iacc, (convf, conva):: convacc, alacc)
              case TypeRef(_, ASKLIST, List(askl)) => (iacc, convacc, askl :: alacc)
              case _ => (iacc, convacc, alacc)
            }
        }
      case other =>
        c.abort(c.enclosingPosition, s"I don't know how to extract Needs from $other (${showRaw(other)})")
    }
  }

  def implicitMaps(
    fType: Type, 
    types: List[Type]
  ): Tree = {

    @tailrec
    def inner(types: List[Type], acc: Tree): Tree = types match {
      case v::x =>
        val resultingType = injectType(fType, v)
        inner(x, q"(implicitly[izumi.reflect.Tag[$v]].tag, implicitly[$resultingType]) :: $acc")
      case Nil => acc
    }

    val s = inner(types, q"Nil")
    q"Map($s :_*)"
  }

  /** Given a unary type constructor and a proper type will apply that
    * type to the constructor and return the result.
    * 
    * e.g. injectType(Either[?, Int], String) == Either[String, Int]
    */
  def injectType(
    higherKind: Type,
    inject: Type
  ): Type = {
    val (name: Symbol, newTypeParams: List[Type]) = higherKind match {
      case PolyType(param :: Nil, TypeRef(_,_,allParams)) => (
        higherKind.typeSymbol,
        allParams.map{ case TypeRef(_,`param`,_) => inject; case x => x }
      )
      case TypeRef(_,name,_) =>
        (name, List(inject))
      case bad =>
        c.abort(c.enclosingPosition, s"$bad is not of kind * -> *")
    }
    tq"${name}[..$newTypeParams]".toType
  }

  def injectType2(
    higherKind: Type,
    injectOne: Type,
    injectTwo: Type    
  ): Type = {
    val (name: Symbol, newTypeParams: List[Type]) = higherKind match {
      case TypeRef(_,name,_) =>
        (name, List(injectOne, injectTwo))
      case PolyType(paramA :: paramB :: Nil, TypeRef(_,_,allParams)) => (
        higherKind.typeSymbol,
        allParams.map{
          case TypeRef(_,`paramA`,_) => injectOne
          case TypeRef(_,`paramB`,_) => injectTwo
          case x => x
        }
      )
      case bad =>
        c.abort(c.enclosingPosition, s"$bad is not of kind * -> * -> *")
    }
    tq"${name}[..$newTypeParams]".toType
  }

  implicit class RichTree(t: Tree) {
    def toType: Type =
      c.typecheck(q"??? : $t").tpe    
  }

  // exists only for testing injectType
  def coldImplicit[A, TC[_]](
    implicit tta: WeakTypeTag[A],    
    ttn: WeakTypeTag[TC[_]]
  ): Expr[TC[A]] = {
    c.Expr(
      c.inferImplicitValue(injectType(ttn.tpe, tta.tpe), false)
    )
  }

  // E[A] => F[A] // where F is interpreted type (perhaps WebMonad)
  // Future[A] => WebMonad[A]
  /** for {
    *    a <- convert(callApiOne()) // Future[Int]
    *    b <- convert(callApiTwo()) // IO[String]
    * }
    */
  def getConvMap(fType : Type, eTypes: List[(Type, Type)]): Tree = {
    val mapElems = eTypes.map{
      case (e: Type, a: Type) =>
        q"(implicitly[izumi.reflect.TagK[${e}]].tag, implicitly[izumi.reflect.Tag[${a}]].tag) -> ((implicitly[ltbs.uniform.Converter[$e, $fType, $a]]): Any)"
      case (bad, _) =>
        c.abort(c.enclosingPosition, s"$bad is not of kind * -> * (${showRaw(bad)})")
    }
    q"Map( ..$mapElems )"
  }

  def getInteractMap(fType : Type, eTypes: List[(Type, Type)]): Tree = {
    val mapElems = eTypes.map{
      case (t: Type, a: Type) =>
        val key = q"(implicitly[izumi.reflect.Tag[${t}]].tag, implicitly[izumi.reflect.Tag[${a}]].tag)"
        val faType = injectType2(fType, t, a)
        val value = q"((implicitly[$faType]))"
        q"$key -> $value"
      case (bad, _) =>
        c.abort(c.enclosingPosition, s"$bad is not of kind * -> * (${showRaw(bad)})")
    }
    q"Map( ..$mapElems )"
  }

  def interpreter_impl[H <: Needs[_, _], A, INTERACTTC[_,_], ASKLISTTC[_], F[_], T](
    program: Expr[Uniform[H,T,A]]
  )(
    implicit ttn: WeakTypeTag[H], 
    ttInteractTc: WeakTypeTag[INTERACTTC[_,_]],
    ttAskListTc: WeakTypeTag[ASKLISTTC[_]],    
    fTypeTag: WeakTypeTag[F[_]]
  ): c.Expr[F[A]] = {
    val (interactTypes, convTypes, listTypes) = getNeeds[H, INTERACTTC, ASKLISTTC]
    val interactMap = getInteractMap(ttInteractTc.tpe, interactTypes)
    val listMap = implicitMaps(ttAskListTc.tpe, listTypes)    
    val convMap = getConvMap(fTypeTag.tpe, convTypes)
    val r = q"${c.prefix}.interpretImpl($program, $interactMap, $convMap, $listMap)"
    c.Expr[F[A]](r)
  }

}
