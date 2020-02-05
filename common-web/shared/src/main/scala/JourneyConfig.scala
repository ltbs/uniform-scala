package ltbs.uniform
package common.web

/**
  *
  * @param leapAhead
  *                  fast forward to the first empty question in a journey
  * @param askFirstListItem
  *                         when asking for list A with a minimum number of A required then go straight to add A subjourney
  *                         rather than to the empty listing index page
  */
case class JourneyConfig(
  leapAhead: Boolean = true,
  askFirstListItem: Boolean = false
)
