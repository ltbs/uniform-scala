package ltbs.uniform.gformsparser

sealed trait Address {
  def line1: String
  def line2: Option[String]
  def line3: Option[String]
  def line4: Option[String]
}

case class DomesticAddress(
  line1: String,
  line2: Option[String],
  line3: Option[String],
  line4: Option[String],
  postcode: String
) extends Address

case class InternationalAddress(
  line1: String,
  line2: Option[String],
  line3: Option[String],
  line4: Option[String],
  country: String
) extends Address
