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

  type WindowsTaxStack = Fx.fx4[
    UniformAsk[List[Window], ?],
    UniformAsk[Int, ?],
    UniformAsk[(Int,Int), ?],
    UniformAsk[Orientation, ?]
  ]

  def program[R
      : _uniformAsk[List[Window],?]
  ]: Eff[R, Int] =
    for {
      windows <- uask[List[Window], R]("windows")
    } yield windows.size

  def singleWindowProgram[R
      : _uniformAsk[Int, ?]
      : _uniformAsk[(Int,Int), ?]
      : _uniformAsk[Orientation, ?]
      : _uniformAsk[Boolean, ?]
  ](
    key: String,
    existing: List[Window],
    default: Option[Window]
  ): Eff[R, Window] = 
    (
      uask[Int,R]("floor"),
      uask[(Int,Int),R]("dimensions"),
      uask[Int,R]("yearsInPlace") emptyUnless uask[Boolean,R]("existing"),
      uask[Orientation,R]("orientation")
    ).mapN(Window)

}
