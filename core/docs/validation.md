---
layout: docs
title: Validation
---

# Validation

For any given `interact[Tell, Ask]` the validation data structure is
`List[List[Rule[Ask]]]`.

Each inner groups of Rules is carried out 'concurrently' (error
accumulating) before sequentially passing validation on to the next
stage (fail-fast).

This means that error checking can be applied in-order, all together
or some mix of the two. For example, you might want to check that a
user supplies valid data on all the fields in an address. It would be
annoying for the user if they corrected an error on the first field
then resubmitted before seeing an error on the second field. In this
case you'd want the errors to accumulate. Once all the initial checks
pass you might want to then run a check afterwards, for example to
ensure the address actually exists or can be delivered to.

Lets start with an example with no validation at all -

```tut:silent
import ltbs.uniform._

case class Address(
    line1: NonEmptyString,
    line2: String,
    line3: String,
    line4: String,
    postcode: NonEmptyString
) {
    def lines: List[String] =
      List(line1, line2, line3, line4, postcode)
}


type AskTypes = Address :: NilTypes
type TellTypes = NilTypes

def askAddress1[F[_]](
  interpreter: Language[F, TellTypes, AskTypes]
): F[Address] =
  interpreter.ask[Address]("post-to")
```

We can start with a single rule, a simple regex check against a postcode -

```tut:silent
import cats.data.NonEmptyList

val regex = "^[A-Z]{1,2}\\d[A-Z\\d]? ?\\d[A-Z]{2}$"

val postcodeCheck = Rule.pattern[Address] {
  case x if !x.postcode.matches(regex) ⇒
    (ErrorMsg("bad-postcode"), NonEmptyList.one(List("postcode")))
}
```

Here we have given a scenario for an error as a partial function. It
is important to remember we are dealing with negatives - matching
against a bad input rather than giving a predicate for a good
record.

The regex is used as a guard and an `ErrorMsg` is supplied
along with a non-empty path to where on the form/input data structure
the error applies. There can be several paths because in some cases
the error may be applicable to several fields - if the town doesn't
match the postcode we might want to display the error on both those
fields.

We can now test our rule on the REPL or in a unit test -

```tut
val testAddress: Address = Address(
  NonEmptyString("12 The Street"),
  "Genericford",
  "Madeupshire",
  "",
  NonEmptyString("BAD POSTCODE")
)

postcodeCheck.apply(testAddress)
```

If we want to apply our validation rule to a step in a journey we
simply supply it as a parameter.

```tut:silent
def askAddress2[F[_]](
  interpreter: Language[F, TellTypes, AskTypes]
): F[Address] =
  interpreter.ask[Address]("post-to", validation = postcodeCheck)
```

In this case we only have a single `Rule` applied to the `validation`
parameter, however Uniform will implicitly convert a solo `Rule` to a
`List[List[Rule]]` for us.

If we wanted to check both a postcode against a Regex and that the 1st
line starts with a number we might be tempted to do something like this -

```tut:silent
val dontDoThis = Rule.pattern[Address] {
    case x if !x.postcode.matches(regex) ⇒
      (ErrorMsg("bad-postcode"), NonEmptyList.one(List("postcode")))
    case Address(line1, _, _, _, _) if !line1.head.isDigit ⇒
      (ErrorMsg("line1-too-long"), NonEmptyList.one(List("line1")))
}
```

However because we are passing in a single pattern match only the
first error case would apply.

```tut
val testAddress: Address = Address(
  NonEmptyString("Fred"),
  "Genericford",
  "Madeupshire",
  "",
  NonEmptyString("BAD POSTCODE")
)

dontDoThis.apply(testAddress)
```

If we wanted to assert them together we would need to create a group of
rules.

```tut:silent
val postcodeCheck = Rule.pattern[Address] {
  case x if !x.postcode.matches(regex) ⇒
    (ErrorMsg("bad-postcode"), NonEmptyList.one(List("postcode")))
}

val line1Check = Rule.pattern[Address] {
    case Address(line1, _, _, _, _) if !line1.head.isDigit ⇒
      (ErrorMsg("line1-too-long"), NonEmptyList.one(List("line1")))
}

val errorAccumulating = List(List(postcodeCheck, line1Check))
val failFast = List(List(postcodeCheck), List(line1Check))
```

Should you want to test your rules you can turn a
`List[List[Rule[A]]]` into a `Rule[A]` with the `combined` method for
ready use on the REPL or inside of unit tests.

```tut
errorAccumulating.combined(testAddress)
failFast.combined(testAddress)
```

## Special Rules

You may be wondering why the rules are not functions like `A ⇒
Either[ErrorTree, A]` which could automatically compose without the
need for the `combined` method above.

The reason for this is that we
have no way to decompose a function back again into its constituent
parts, and we may want to know what the parts are in order to offer
extra guidance to the user. For example, a textarea page in a web
representation may have a javascript hint for the number of characters
remaining.

```tut
Rule.max[String](5).apply("too long")
Rule.min[List[Boolean]](min = 100).apply(Nil)
Rule.size[String](min = 1, max = 12).apply("just right")
```
