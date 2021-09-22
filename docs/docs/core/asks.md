---
layout: docs
title: Asking
---

> **An `ask` is a step in a Uniform journey that requests information from the user.**

# Asking

Whereas a `tell` represents sending typed information to the user an
`ask` is the reverse - it represents prompting the user to supply some
data.

For example to ask the user to enter an `Int` value with a step id of
"age" we would type -

```scala
import ltbs.uniform._

ask[Int]("age")
```


## default values

We can supply a default value for an `ask`, and this can depend upon
other values in scope -

```scala
val defaultAge = 39
ask[Int]("age", default = Some(defaultAge))
```

# validation rules

Values can be validated by specifying one or more typed `Rule`s, these
can be used to restrict any value that the user inputs via an `ask`,
and can be composed of other rules to provide more complex behaviour. 

At it's simplest a `Rule` is a test (predicate) against the value supplied
by a user and an error message key that is emitted if test fails.

## existing validation rules

There are a number of predefined `Rule`s you can pick from -

```scala
import ltbs.uniform.validation._

ask[String]("name", validation = Rule.nonEmpty)
ask[Int]("age", validation = Rule.between(18, 120))
```

## bespoke validation rules

Alternatively you can create completely custom `Rule`s -

```scala
val postcodeRegex = "^[A-Z]{1,2}\\d[A-Z\\d]? ?\\d[A-Z]{2}$"
val postcodeCheck = Rule.cond[String](
  _.matches(postcodeRegex),
  "bad-postcode"
)

ask[String]("postcode", validation = postcodeCheck)
```
## validation rules on the REPL

You can also test the validation rules in isolation, if you wish -

```scala
postcodeCheck.apply("AB12 3CD")
// res5: cats.data.Validated[ErrorTree, String] = Valid("AB12 3CD")
postcodeCheck.apply("BAD POSTCODE")
// res6: cats.data.Validated[ErrorTree, String] = Invalid(
//   ListMap(
//     NonEmptyList(List(), List()) -> NonEmptyList(ErrorMsg("bad-postcode", WrappedArray()), List())
//   )
// )
Rule.lengthBetween[String](1,30).apply("acceptable")
// res7: cats.data.Validated[ErrorTree, String] = Valid("acceptable")
```

## combining validation rules

Sometimes you may wish to use several validation rules together for a
single value, for example if we wish to check that a day is in the
past and that it is a weekday we might use two rules as follows -

```scala
import java.time.LocalDate


val weekdayRule = Rule.cond[LocalDate](
  _.getDayOfWeek.getValue < 6,
  "not-a-weekday"
)

// add an order instance to allow LocalDate to be used with max
implicit val orderDate = cats.Order.from[LocalDate]{
  case (a,b) => a.toEpochDay compare b.toEpochDay
}

val inPastRule = Rule.max(LocalDate.now.minusDays(1))
```

We could combine these rules sequentially by using
`followedBy` -

```scala
ask[String](
  "postcode", 
  validation=Rule.nonEmpty[String] followedBy postcodeCheck
)
```

In this situation the second rule will only be evaluated if the first
rule passes (fail fast semantics). You may wish to do this if it only
makes sense to display a single error message. In the above example if
the user inputs an empty string we would only want them to see a single 
error message, not both. 

However you may wish for the two be applied together regardless (error
accumulating semantics), for this you would use `alongWith` -

```scala
ask[LocalDate](
  "start-date", 
  validation=weekdayRule alongWith inPastRule
)
```

In this case we want the user to be notified as such if the address they 
enter is both not a weekday and also not in the past, this saves the user
from having to correct one problem and resubmit only to be told there is 
another error.

## validation rules for compound types 

You may wish to write a validation rule for a complex datatype, for
example lets suppose that we want to create an address type containing
a postcode -

```scala
case class Address(
    line1: String,
    line2: String,
    line3: String,
    postcode: String
)

val badAddress: Address = Address(
  "1 The Commons",
  "Genericford",
  "Madeupshire",
  "BAD POSTCODE"
)

val goodAddress: Address = Address(
  "The Scottish Parliament",
  "",
  "Edinburgh",
  "EH99 1SP"
)

```

We can now create a test that inspects the postcode within the
address - 

```scala
val addressCheck = Rule.cond[Address](
  _.postcode.matches(postcodeRegex),
  "bad-postcode"
)
```

Errors are formed into an `ErrorTree`, which means they have a position
in a hierarchy that mirrors the datatype being tested.

When calling `apply` on a `Rule` we will get back a
`cats.data.Validated`, the right side will contain the object
returned unchanged if the validation passes, and the left side will
contain an `ErrorTree` if the validation fails. 

```scala
import cats.data.Validated

val Validated.Valid(result) = addressCheck(goodAddress)
// result: Address = Address(
//   "The Scottish Parliament",
//   "",
//   "Edinburgh",
//   "EH99 1SP"
// )
result == goodAddress
// res10: Boolean = true

// an ErrorTree is a 'many to many' relationship, the same error 
// can have many paths, and a path can have many errors
val Validated.Invalid(errMsg) = addressCheck(badAddress)
// errMsg: collection.immutable.ListMap[cats.data.NonEmptyList[List[String]], cats.data.NonEmptyList[ErrorMsg]] = ListMap(
//   NonEmptyList(List(), List()) -> NonEmptyList(ErrorMsg("bad-postcode", WrappedArray()), List())
// )
```

In this case the error will appear at the root of the `ErrorTree`. It
may be that we would instead prefer the error to appear connected to
the postcode property, in which case we could re-write our `Rule` as
follows - 

```scala
val addressCheck2 = Rule.condAtPath[Address]("postcode")(
  _.postcode.matches(postcodeRegex),
  "bad-postcode"
)

val Validated.Invalid(errMsg2) = addressCheck2(badAddress)
```
