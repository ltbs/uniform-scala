package ltbs.uniform
package examples.dst

sealed trait Activity
object Activity {
  def values: Set[Activity] = Set(
    SocialMedia, SearchEngine, OnlineMarketplace
  )

  case object SocialMedia extends Activity
  case object SearchEngine extends Activity
  case object OnlineMarketplace extends Activity  
}
