package ltbs.uniform.html

import scalatags._

class SharedTemplates[Builder, Output <: FragT, FragT](
  val bundle: generic.Bundle[Builder, Output, FragT]
) {
  import bundle.all._
  val widget: Tag = div("hello")
}
