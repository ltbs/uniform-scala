package ltbs.uniform
package examples.witchcraft

case class WitchReport(
  accused: Accused,
  evidence: List[Evidence],
  familiars: List[Familiar]
)
