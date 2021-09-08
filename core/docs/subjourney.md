---
layout: docs
title: Subjourneys
---

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


