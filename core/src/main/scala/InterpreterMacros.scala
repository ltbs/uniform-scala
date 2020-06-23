package ltbs.uniform

import cats.instances.either._
import cats.instances.list._
import cats.syntax.alternative._
import com.github.ghik.silencer.silent
import scala.language.higherKinds
import scala.reflect.macros.whitebox
import scala.annotation.tailrec
import izumi.reflect.TagK

class InterpreterMacros(val c: whitebox.Context) {
  import c.universe._

  /**
    * Turn a Needs[_] type into a (List[AskTypes], List[TellTypes]) 
    */ 
  @silent("never used") // quasiquoting seems to produce lots of false warnings  
  def getNeeds[H <: Needs[_], ASKTC[_], TELLTC[_]](
    implicit ttn: c.WeakTypeTag[H], 
    ttAskTc: WeakTypeTag[ASKTC[_]],
    ttTellTc: WeakTypeTag[TELLTC[_]]
  ): (List[Type], List[Type], List[Type]) = {

    val ASK = symbolOf[Needs.Ask[_]]
    val TELL = symbolOf[Needs.Tell[_]]
    val CONV = symbolOf[Needs.Convert[_]]    
    
    ttn.tpe match {
      case _: ExistentialType => (Nil, Nil, Nil)
      case TypeRef(_, ASK, List(ask)) => (ask :: Nil, Nil, Nil)
      case TypeRef(_, TELL, List(tell)) => (Nil, tell :: Nil, Nil)
      case TypeRef(_, CONV, List(conv)) => (Nil, Nil, conv :: Nil)        
      case RefinedType(parents,_) =>
        val ret = parents.collect{
          case TypeRef(_, ASK, List(ask)) => Left(Left(ask))
          case TypeRef(_, TELL, List(tell)) => Left(Right(tell))
          case TypeRef(_, CONV, List(conv)) => Right(conv)
        }
        val (at, c) = ret.separate
        val (a, t) = at.separate
        (a, t, c)
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
        val resultingType = swapType(fType, v)
        inner(x, q"(implicitly[izumi.reflect.Tag[$v]].tag, implicitly[$resultingType]) :: $acc")
      case Nil => acc
    }

    val s = inner(types, q"Nil")
    q"Map($s :_*)"
  }

  /** Given a unary type constructor and a proper type will apply that
    * type to the constructor and return the result.
    * 
    * e.g. swapType(Either[?, Int], String) == Either[String, Int]
    */
  def swapType(
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

  implicit class RichTree(t: Tree) {
    def toType: Type =
      c.typecheck(q"??? : $t").tpe    
  }

  // exists only for testing swapType
  def coldImplicit[A, TC[_]](
    implicit tta: WeakTypeTag[A],    
    ttn: WeakTypeTag[TC[_]]
  ): Expr[TC[A]] = {
    c.Expr(
      c.inferImplicitValue(swapType(ttn.tpe, tta.tpe), silent = false)
    )
  }

  def getConvMap(fType : Type, eTypes: List[Type]): Tree = {
    val mapElems = eTypes.map{ hx =>
      val h = hx match {
        case ExistentialType(_ :: Nil, api: TypeApi) =>
          api.typeArgs match {
            case _ :: Nil => tq"${hx.typeSymbol}"
            case manyArgs =>
              val names: List[Tree] = manyArgs.map{
                case TypeRef(NoPrefix, _, List()) => tq"A"
                case x => tq"$x"
              }
              tq"({type L[A] = ${hx.typeSymbol}[..${names}]})#L"
          }
        case bad =>
          c.abort(c.enclosingPosition, s"$bad is not of kind * -> * (${showRaw(bad)})")
      }
      q"implicitly[izumi.reflect.TagK[${h}]].tag -> (implicitly[cats.~>[${h},$fType]] : Any)"
    }
    q"Map( ..$mapElems )"
  }

  def interpreter_impl[H <: Needs[_], A, ASKTC[_], TELLTC[_], F[_], T](
    program: Expr[Uniform[H,A,T]]
  )(
    implicit ttn: WeakTypeTag[H], 
    ttAskTc: WeakTypeTag[ASKTC[_]],
    ttTellTc: WeakTypeTag[TELLTC[_]],
    fTypeTag: WeakTypeTag[F[_]]
  ): c.Expr[F[A]] = {
    val (askTypes, tellTypes, convTypes) = getNeeds[H, ASKTC, TELLTC]
    val askMap = implicitMaps(ttAskTc.tpe, askTypes)
    val tellMap = implicitMaps(ttTellTc.tpe, tellTypes)
    val convMap = getConvMap(fTypeTag.tpe, convTypes)
    val r = q"${c.prefix}.interpretImpl($program, $askMap, $tellMap, $convMap)"
    c.Expr[F[A]](r)
  }

}
