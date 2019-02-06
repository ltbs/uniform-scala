package ltbs.uniform.sampleprograms


import cats.implicits._
import org.atnos.eff._
import ltbs.uniform._
import enumeratum._

object WindowTax {

  sealed trait Orientation extends EnumEntry
  object Orientation extends Enum[Orientation] {
    val values = findValues
    case object North extends Orientation
    case object South extends Orientation
    case object West  extends Orientation
    case object East  extends Orientation
  }  

  case class Window(
    floor: Int,
    dimensions: (Int,Int),
    yearsInPlace: Int,
    orientation: Orientation
  )

  type WindowsTaxStack = Fx1[
    Uniform[Unit,List[Window], ?]
  ]

  def program[R
      : _uniformAsk[List[Window],?]
  ]: Eff[R, Int] =
    for {
      windows <- uask[List[Window], R]("windows")
    } yield windows.size

}
