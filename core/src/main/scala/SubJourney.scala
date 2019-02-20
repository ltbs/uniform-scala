package ltbs.uniform
import org.atnos.eff.Eff

case class SubJourneySettings (
  leapAhead: Boolean = false,
  amnesiac: Boolean = false
)

object SubJourneySettings {
  def default = SubJourneySettings()
}

case class SubJourney[STACK,A](
  path: String,
  settings: SubJourneySettings = SubJourneySettings.default,
  journey: Eff[STACK,A]
)
