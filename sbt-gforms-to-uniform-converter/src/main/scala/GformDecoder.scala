package ltbs.uniform.gformsparser

import cats.implicits._
import pureconfig._
//import java.net.URL
import pureconfig.error.ConfigReaderFailures

case class GformTemplate(
  _id: String,
  formName: String,
  description: String = "",
  submissionReference: String = "$user.entityId",
  draftExpiryDays: Int = 28,
  ttlDays: Option[Int],
  archived: Boolean = false,
  characterSet: Option[String],
  dmsSubmission: DmsSubmission,
  authConfig: AuthConfig,
  destinations: List[String] = Nil,
  destinationConfig: Option[String],
  emailTemplateId: Option[String],
  submitSuccessUrl: Option[URL],
  submitErrorUrl: Option[URL],
  preAuthSections: List[StandardSection] = Nil,
  sections: List[StandardSection],
  acknowledgementSection: EndSection,
  declarationSection: EndSection
) {
  def allSections: List[Section] =
    sections ++ List(acknowledgementSection, declarationSection)

  def deduplicatedSectionId(section: Section): String = {
    val letters = ('A' to 'Z').toList
    allSections.filter(_.id == section.id) match {
      case `section` :: Nil => section.id
      case others => section.id :+ letters(others.indexOf(section))
    }
  }

  def findSection(f:Field): Option[Section] = {
    allSections.find(_.fields.contains(f))
  }
}

case class DmsSubmission (
  dmsFormId: Option[String],
  customerId: String,
  classificationType: String,
  businessArea: String
)

trait Section {
  def title : String
  def fields: List[Field]
  def repeatsMin: Option[String]
  def repeatsMax: Option[String]
  def includeIf: Option[String]

  def isList: Boolean = repeatsMax.isDefined || repeatsMin.isDefined

  /** As a last resort we could take the title, however this is
    * probably very long and possibly not unique
    */
  private def desperateId: String =
    title.trim.split(" ").toList.lowerCamel

  /** If there is only a single field with a sane Id, use that field
    * id as the id for the section
    */
  private def firstFieldId: Option[String] =
    fields.filterNot(_.isInstanceOf[InfoField])
      .flatMap(_.saneId.toList) match {
        case x :: Nil => Some(x)
        case _ => None
      }

  def id: String = firstFieldId getOrElse desperateId

  def infoFields: List[InfoField] = fields.collect{case x: InfoField => x}
}

case class StandardSection(
  title: String,
  fields: List[Field],
  includeIf: Option[String],
  repeatsMin: Option[String] = None,
  repeatsMax: Option[String] = None
) extends Section

case class EndSection(
  title: String,
  shortName: Option[String],
  fields: List[Field],
  repeatsMin: Option[String] = None,
  repeatsMax: Option[String] = None
) extends Section {
  def includeIf: Option[String] = None
}


case class AuthConfig(
  authModule: String,
  confidenceLevel: Int = 50,
  permittedAffinityGroups: List[String] = List("Organisation"),
  serviceId: Option[String],
  regimeId: Option[String],
  repeatsMin: Option[String] = None,
  repeatsMax: Option[String] = None
)
