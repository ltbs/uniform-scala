  // {
  //   case class AttrPair[A](attribute: Attribute[A]) {
  //     type ValueType
  //     val value: ValueType
  //   }

  //   trait Attribute[A] {
  //     type AcceptableValue

  //     def :=(valueIn: AcceptableValue): AttrPair[A] = {
  //       val attr = this
  //       new AttrPair[A](attr) {
  //         type ValueType = AcceptableValue
  //         val value = valueIn
  //       }
  //     }
  //   }

  //   case class Default[A]() extends Attribute[A] {
  //     type AcceptableValue = A
  //   }
  //   def defaultValue[A] = new Default[A]

  //   def validating[A] = new Attribute[A] {
  //     type AcceptableValue = A => Boolean
  //   }
    
  //   val l : List[AttrPair[String]] = List(
  //     defaultValue := "test",
  //     validating := {_.startsWith("test")}
  //   ) // doesn't compile

  //   val default: Option[String] = l collectFirst {
  //     case a@AttrPair(Default()) => a.value // won't compile (compiler doesn't know String =:= ValueType
  //   }
  // }
