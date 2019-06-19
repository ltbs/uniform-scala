package ltbs.uniform
package common.web

trait InferFormGroup[Html] {
  type FF[A] = FormField[A, Html]
}
