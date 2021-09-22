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

```scala mdoc
import ltbs.uniform._, validation._

case class Address(
    line1: String,
    line2: String,
    line3: String,
    line4: String,
    postcode: String
) {
    def lines: List[String] =
      List(line1, line2, line3, line4, postcode)
}

def askAddress1 = ask[Address]("post-to")
```

We can start with a single rule, a simple regex check against a postcode -

```scala mdoc:silent
import cats.data.NonEmptyList

val regex = "^[A-Z]{1,2}\\d[A-Z\\d]? ?\\d[A-Z]{2}$"
val postcodeCheck = Rule.cond[Address](_.postcode.matches(regex), "bad-postcode")
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

```scala mdoc
val testAddress: Address = Address(
  "12 The Street",
  "Genericford",
  "Madeupshire",
  "",
  "BAD POSTCODE"
)

postcodeCheck.apply(testAddress)
```

If we want to apply our validation rule to a step in a journey we
simply supply it as a parameter.

```scala mdoc:silent
def askAddress2 =
  ask[Address]("post-to", validation = postcodeCheck)
```

In this case we only have a single `Rule` applied to the `validation`
parameter.

If we wanted to check both a postcode against a Regex and that the 1st
line starts with a number we can either do this sequentially using
`followedBy` - 

```scala mdoc:silent
val sequentialChecks: Rule[Address] = 
  postcodeCheck followedBy 
    Rule.cond[Address](_.line1.head.isDigit, "line-must-start-with-number")
```
