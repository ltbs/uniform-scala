package ltbs.uniform.gformsparser
import enumeratum._

sealed trait DraftRetrievalMethod extends EnumEntry with EnumEntry.Uncapitalised
object DraftRetrievalMethod extends Enum[DraftRetrievalMethod] {

  lazy val values = findValues

  case object OnePerUser extends DraftRetrievalMethod
  case object FormAccessCode extends DraftRetrievalMethod
  case object FormAccessCodeForAgents extends DraftRetrievalMethod
  case object NotPermitted extends DraftRetrievalMethod
}
