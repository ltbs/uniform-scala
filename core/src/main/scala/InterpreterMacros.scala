package ltbs.uniform

import cats.instances.either._
import cats.instances.list._
import cats.syntax.alternative._
import com.github.ghik.silencer.silent
import scala.language.higherKinds
import scala.reflect.macros.blackbox
import scala.annotation.tailrec

class InterpreterMacros(val c: blackbox.Context) {
  import c.universe._

  /**
    * Turn a Needs[_] type into a (List[AskTypes], List[TellTypes]) 
    */ 
  @silent("never used") // quasiquoting seems to produce lots of false warnings  
  def getNeeds[H <: Needs[_], ASKTC[_], TELLTC[_], ASKLISTTC[_]](
    implicit ttn: c.WeakTypeTag[H], 
    ttAskTc: WeakTypeTag[ASKTC[_]],
    ttTellTc: WeakTypeTag[TELLTC[_]],
    ttAskListTc: WeakTypeTag[ASKLISTTC[_]]
  ): (List[Type], List[Type], List[(Type, Type)], List[Type]) = {

    val ASK = symbolOf[Needs.Ask[_]]
    val TELL: TypeSymbol = symbolOf[Needs.Tell[_]]
    val CONV = symbolOf[Needs.Convert[Any,_]]
    val ASKLIST = symbolOf[Needs.AskList[_]]        


    ttn.tpe match {
      case _: ExistentialType => (Nil, Nil, Nil, Nil)
      case TypeRef(_, ASK, List(ask)) => (ask :: Nil, Nil, Nil, Nil)
      case TypeRef(_, TELL, List(tell)) => (Nil, tell :: Nil, Nil, Nil)
      case TypeRef(_, CONV, List(convf, conva)) => (Nil, Nil, (convf, conva) :: Nil, Nil)
      case TypeRef(_, ASKLIST, List(askl)) => (Nil, Nil, Nil, askl :: Nil)                
      case RefinedType(parents,_) =>
        //TODO - a fold would be better
        val ret = parents.collect{
          case TypeRef(_, ASK, List(ask)) => Left(Left(ask))
          case TypeRef(_, TELL, List(tell)) => Left(Right(tell))
          case TypeRef(_, CONV, List(convf, conva)) => Right(Left((convf, conva)))
          case TypeRef(_, ASKLIST, List(askl)) => Right(Right(askl))            
        }
        val (at, cl) = ret.separate
        val (a, t) = at.separate
        val (c, l) = cl.separate        
        (a, t, c, l)
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
      c.inferImplicitValue(swapType(ttn.tpe, tta.tpe), false)
    )
  }

  def getConvMap(fType : Type, eTypes: List[(Type, Type)]): Tree = {
    val mapElems = eTypes.map{
      case (e: Type, a: Type) =>
        q"(implicitly[izumi.reflect.TagK[${e}]].tag, implicitly[izumi.reflect.Tag[${a}]].tag) -> ((implicitly[ltbs.uniform.Converter[$e, $fType, $a]]): Any)"
    }
    q"Map( ..$mapElems )"
  }


  def getConvMapP(fType : Type, eTypes: List[(Type, Type)]): Tree = {
    val mapElems = eTypes.map{ case (hx, _) =>
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


  def interpreter_impl[H <: Needs[_], A, ASKTC[_], TELLTC[_], ASKLISTTC[_], F[_], T](
    program: Expr[Uniform[H,A,T]]
  )(
    implicit ttn: WeakTypeTag[H], 
    ttAskTc: WeakTypeTag[ASKTC[_]],
    ttTellTc: WeakTypeTag[TELLTC[_]],
    ttAskListTc: WeakTypeTag[ASKLISTTC[_]],    
    fTypeTag: WeakTypeTag[F[_]]
  ): c.Expr[F[A]] = {
    val (askTypes, tellTypes, convTypes, listTypes) = getNeeds[H, ASKTC, TELLTC, ASKLISTTC]
    val askMap = implicitMaps(ttAskTc.tpe, askTypes)
    val tellMap = implicitMaps(ttTellTc.tpe, tellTypes)
    val listMap = implicitMaps(ttAskListTc.tpe, listTypes)    
    val convMap = getConvMap(fTypeTag.tpe, convTypes)
    val r = q"${c.prefix}.interpretImpl($program, $askMap, $tellMap, $convMap, $listMap)"
    c.Expr[F[A]](r)
  }

}
