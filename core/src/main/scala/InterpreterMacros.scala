package ltbs.uniform

import cats.instances.either._
import cats.instances.list._
import cats.syntax.alternative._
import com.github.ghik.silencer.silent
import scala.language.higherKinds
import scala.reflect.macros.whitebox
import scala.annotation.tailrec

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
  ): (List[Type], List[Type]) = {
    ttn.tpe match {
      case _: ExistentialType => (Nil, Nil)
      case RefinedType(parents,_) =>
        // parents.zipWithIndex.foreach{ case (p, i) => 
        //   c.info(c.enclosingPosition, s"$i: " ++ showRaw(p), true)
        // }

        val ASK = symbolOf[Needs.Ask[_]]
        val TELL = symbolOf[Needs.Tell[_]]        
        val ret = parents.collect{
          case TypeRef(_, ASK, List(ask)) => Left(ask)
          case TypeRef(_, TELL, List(tell)) => Right(tell)            
        }
        ret.separate
      case other =>
        c.abort(c.enclosingPosition, s"I don't know how to extract Needs from $other")
    }
  }

  def implicitMaps(
    fType: Type, 
    types: List[Type]
  ): Tree = {

    val symbol = fType.typeSymbol

    val typeArgs: List[Type] = fType.typeArgs.takeWhile(_.toString != "Any")

    @tailrec
    def inner(types: List[Type], acc: Tree): Tree = types match {
      case v::x => inner(x, q"(implicitly[izumi.reflect.Tag[$v]].tag, implicitly[${symbol}[..$typeArgs, $v]]) :: $acc")
      case Nil => acc
    }

    val s = inner(types, q"Nil")
    q"Map($s :_*)"
  }

  def coldImplicit[A, TC[_]](
    implicit tta: WeakTypeTag[A],    
    ttn: WeakTypeTag[TC[_]]
  ): Expr[TC[A]] = {

    //    c.info(c.enclosingPosition, s"TTA: ${tta.tpe} /" + showRaw(tta.tpe), true)
    val a: Type = tta.tpe

    def finalType = {
      // c.info(c.enclosingPosition, s"TTN.raw: ${showRaw(ttn.tpe)}", true)      
      // c.info(c.enclosingPosition, s"TTN.tpe.typeArgs: ${ttn.tpe.typeArgs}", true)
      // c.info(c.enclosingPosition, s"TTN.tpe.typeSymbol.asType.typeParams: ${ttn.tpe.typeSymbol.asType.typeParams}", true)

      val newTypeParams: List[Type] = ttn.tpe match {
        case PolyType(param :: Nil, TypeRef(_,_,allParams)) =>
          c.info(c.enclosingPosition, s"param: ${showRaw(param)}", true)
          c.info(c.enclosingPosition, s"allParams: ${allParams.map(showRaw(_))}", true)          
          allParams.map{ case TypeRef(_,`param`,_) => a; case x => x }
        case TypeRef(_,_,_) =>
          List(a)
      }

      val unchecked = tq"${ttn.tpe.typeSymbol}[..$newTypeParams]"
//      c.info(c.enclosingPosition, s"TTN: ${ttn.tpe} / " + showRaw(ttn.tpe), true)
//      c.info(c.enclosingPosition, s"Option[String]: " + showRaw(tq"Option[String]"), true)
//      val t = AppliedTypeTree(Ident(TypeName("Option")), List(Ident(TypeName("String"))))
    val q = q"??? : $unchecked"
    c.info(c.enclosingPosition, s"q: ${q}", true)      
      c.typecheck(q).tpe
    }
//    val tree = c.inferImplicitValue(finalType, silent = false)
//    c.Expr(tree)
//    c.Expr(q"implicitly[$finalType]")
    c.Expr(
      c.inferImplicitValue(finalType, silent = false)
    )
    // val typeS = q"implicitly[${tc.typeSymbol}[$a]]"
    // val ttat = c.typecheck(typeS).tpe
    // c.info(c.enclosingPosition, s"TTAT: ${ttat} / " + showRaw(ttat), true)
    // c.inferImplicitValue(ttat, silent = false) match {
    //   case EmptyTree => c.abort(c.enclosingPosition, "No implicit found")
    //   case x => c.Expr(x)
    // }
  }

  def interpreter_impl[H <: Needs[_], A, ASKTC[_], TELLTC[_], F[_], T](
    program: Expr[Uniform[H,A,T]]
  )(
    implicit ttn: WeakTypeTag[H], 
    ttAskTc: WeakTypeTag[ASKTC[_]],
    ttTellTc: WeakTypeTag[TELLTC[_]]
  ): c.Expr[F[A]] = {

//    c.info(c.enclosingPosition, s"H: ${ttn.tpe} / " + showRaw(ttn.tpe), true)  
    val (askTypes, tellTypes) = getNeeds[H, ASKTC, TELLTC]
    // c.info(c.enclosingPosition, s"ASKTC: ${ttAskTc.tpe} /" + showRaw(ttAskTc.tpe), true)
    // c.info(c.enclosingPosition, s"TELLTC: ${ttTellTc.tpe} / " + showRaw(ttTellTc.tpe), true)        

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


}
