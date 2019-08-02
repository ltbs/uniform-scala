package ltbs.uniform.interpreters.playframework

import com.github.ghik.silencer.silent

object Compatibility  {

  @silent("deprecated")
  type PlayController = play.api.mvc.Controller

}
