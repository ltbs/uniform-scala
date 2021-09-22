---
layout: docs
title: Subjourneys and loops
---

> **`subjourney` group steps together inside a journey.**

> **`askList` allows a subjourney to loop, returning multiple items.**

# subjourneys

It is possible to group a series of steps into a subjourney with its
own step ID.

```scala mdoc:silent
import ltbs.uniform._

case class Customer(username: String, age: Int)
case class Order(customer: Customer, units: Int)

def placeOrder = for {
  customer <- subJourney("customer") {
    for {
      u <- ask[String]("username")
      a <- ask[Int]("age")
    } yield Customer(u,a)
  }
  units <- ask[Int]("number-of-units")
} yield Order(customer, units)

```

In effect the step ID's are hierarchical, rather than flat, however it
is entirely at the discretion of the interpreter as to how subjourneys
should be handled.

For example a web-based interpreter may choose to create a URL for the
username page under `/customer/username`, whereas a CLI interpreter
may simply flatten the hierarchy and effectively inline the subjourney.

# askList

Consider the situation where you want to `ask` for a collection -

```scala mdoc:silent
case class User(name: String, age: Int)

def journey = for {
    p <- ask[List[User]]("users")
    _ <- tell("confirm", p)
} yield p
```

Now imagine that you want to separate out asking for the name and age
into separate stages, but still returning `List[User]`. For this
purpose you can use `askList`.

```scala mdoc:silent
def journey2 = for {
    p <- askList[User]("users") {
      case (_: Option[Int], _: List[User]) =>
        for {
          name <- ask[String]("name")
          age <- ask[Int]("age")
        } yield User.apply(name, age)
    }
    _ <- tell("confirm", p)
} yield p
```

There is no `interact` equivalent for `askList` as the interaction
will typically involve showing the user the collection as the user
constructs it.

Additionally as the interpreter may provide the mechanism to edit a
record you may wish to pre-populate the steps with values from the
collection.

For this purpose you can access the index of the record being edited
(or `None` if the user is adding a record), and the collection of
records already in the collection.

```scala mdoc:silent
def journey3 = for {
    p <- askList[User]("users") {
      case (editIndex: Option[Int], existing: List[User]) =>
        val editRecord = editIndex.flatMap(existing.lift)
        for {
          name <- ask[String]("name",
            default = editRecord.map(_.name)
          )
          age <- ask[Int]("age",
            default = editRecord.map(_.age)
          )
        } yield User.apply(name, age)
    }
    _ <- tell("confirm", p)
} yield p
```

You could also use this for cross-record validation -

```scala mdoc:silent
import ltbs.uniform.validation.Rule

def journey4 = for {
    p <- askList[User]("users") {
      case (editIndex: Option[Int], existing: List[User]) =>
        val editRecord = editIndex.flatMap(existing.lift)
        for {
          name <- ask[String]("name",
            validation = Rule.cond[String](
              {n => !existing.exists(_.name == n)},
              "duplicate-name"
            ),
            default = editRecord.map(_.name)
          )
          age <- ask[Int]("age",
            default = editRecord.map(_.age)
          )
        } yield User.apply(name, age)
    }
    _ <- tell("confirm", p)
} yield p
```
