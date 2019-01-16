package ltbs.uniform.interpreters.selenium

import cats.Eval
import org.openqa.selenium.WebDriver
import ltbs.uniform.web.DataParser
import ltbs.uniform.web.Input

trait SeleniumInputer[A] {
  def apply(c: A, driver: WebDriver): Eval[Unit]
}

object SeleniumInputer {
  def automatic[A](
    implicit parser: DataParser[A]
  ): SeleniumInputer[A] = new SeleniumInputer[A] {
    def apply(a: A, driver: WebDriver): Eval[Unit] = {
      val tree: Input = parser.unbind(a)

      ???
    }
  }
}
