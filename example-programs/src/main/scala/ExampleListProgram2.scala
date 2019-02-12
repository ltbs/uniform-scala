package ltbs.uniform.sampleprograms


import cats.implicits._
import org.atnos.eff._
import ltbs.uniform._
import enumeratum._

object WindowTax2 {

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
      : _uniformCore    
      : _uniformAsk[List[Window],?]
  ]: Eff[R, Int] =
    for {
      windows <- ask[List[Window]]("windows")
    } yield windows.size

  def singleWindowProgram[R
      : _uniformCore
      : _uniformAsk[Int, ?]
      : _uniformAsk[(Int,Int), ?]
      : _uniformAsk[Orientation, ?]
      : _uniformAsk[Boolean, ?]
  ](
    existing: List[Window],
    default: Option[Window]
  ): Eff[R, Window] = 
    (
      ask[Int]("floor")
        .defaultOpt(default.map(_.floor)).in[R],

      ask[(Int,Int)]("dimensions")
        .defaultOpt(default.map(_.dimensions)).in[R],

      ask[Int]("yearsInPlace")
        .defaultOpt(default.map(_.yearsInPlace)) emptyUnless
        ask[Boolean]("existing")
        .defaultOpt(default.map(_.yearsInPlace != 0)),

      ask[Orientation]("orientation")
        .defaultOpt(default.map(_.orientation)).in[R]

    ).mapN(Window)

}
