package ltbs.uniform
package interpreters.playframework

import common.web._
import concurrent.Future


sealed trait ListControl

final case object Continue extends ListControl
final case object AddAnother extends ListControl
final case class Edit(ordinal: Int) extends ListControl
final case class Delete(ordinal: Int) extends ListControl

// case class ListingPage[A, Html](
//   addEditJourney: Option[A] => Future[PageOut[List[A],Html]]
// )(
//   implicit listItemTell: GenericWebTell[A, Html],
//   listAsk: GenericWebAsk[ListControl, Html]
// ) extends GenericWebAsk[List[A], Html] {

//   def page(
//     targetId: List[String],
//     currentId: List[String],
//     default: Option[List[A]],
//     validation: List[List[Rule[List[A]]]],
//     config: JourneyConfig,
//     submittedData: Option[Input],
//     path: Path,
//     db: DB,
//     messages: UniformMessages[Html]
//   ): Future[PageOut[List[A],Html]] = (targetId, currentId) match {
//     case (a,b) if a == b => listingSubpage()
//     case (a,b) if a.startsWith(b) => addOrEditSubjourney()
//     case _ => ???
//   }

//   def listingSubpage(): Future[PageOut[List[A],Html]] = ???

//   def addOrEditSubjourney(): Future[PageOut[List[A],Html]] =
//     addEditJourney(None)



// }
