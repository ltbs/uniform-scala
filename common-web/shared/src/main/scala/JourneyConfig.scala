package ltbs.uniform
package common.web

/**
  *
  * @param leapAhead
  *                  enables 'leap ahead' behaviour when requested as a parameter
  * @param askFirstListItem
  *                         when asking for list A with a minimum number of A required then go straight to add A subjourney
  *                         rather than to the empty listing index page
  * @param defaultAsEntry
  *                  treat a default value as though it has been entered by the user (allow bypassing)
  */
case class JourneyConfig(
  leapAhead: Boolean = true,
  askFirstListItem: Boolean = false,
  defaultAsEntry: Boolean = false
)
