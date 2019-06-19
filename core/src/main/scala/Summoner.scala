package ltbs.uniform

import shapeless._, ops.hlist.Selector
import reflect.runtime.universe.WeakTypeTag
import scala.language.higherKinds

@annotation.implicitNotFound(
  "The interpreter cannot find a ${TC} instance for all the datatypes in ${A}"
)
trait Summoner[A <: HList, TC[_]] {
  type Supported = A

  def instanceFor[Subtype](
      implicit selector : Selector[Supported, Subtype],
      tt: WeakTypeTag[Subtype]
  ): TC[Subtype]
}

object Summoner {

  implicit def hNilSummoner[TC[_]] = new Summoner[HNil, TC] {
    def instanceFor[Subtype](
      implicit selector : Selector[Supported, Subtype],
      ttp: WeakTypeTag[Subtype]
    ): TC[Subtype] = throw new IllegalStateException(
      "Not possible!"
    )
  }

  implicit def hConsSummoner[H, T <: HList, TC[_]](
    implicit
      hInstance: Lazy[TC[H]],
    tInstance: Summoner[T, TC],
    tth: WeakTypeTag[H]
  ): Summoner[H :: T, TC] = new Summoner[H :: T, TC] {
    def instanceFor[Subtype](
      implicit selector : Selector[Supported, Subtype],
      ttp: WeakTypeTag[Subtype]
    ): TC[Subtype] = {
      if (ttp.tpe =:= tth.tpe)
        hInstance.value.asInstanceOf[TC[Subtype]]
      else tInstance.instanceFor(
        selector.asInstanceOf[Selector[tInstance.Supported,Subtype]],
        ttp
      )
    }
  }

}
