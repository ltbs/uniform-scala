---
layout: docs
title: Composing journeys
---

# Composing journeys together

A journey is simply a composition of steps (such `ask`, `tell`, `interact` or `end`) 
using the same semantics as monads such as `Option` or `List` that you are 
already familiar with in Scala. 

Lets start with a simple example that collects two `Person` records 
from a user. We can simply put the two `ask` calls together in a for 
comprehension. 

```scala
import ltbs.uniform._

case class Person(name: String, age: Int)

def senderAndReceiver1 = for {
  sender   <- ask[Person]("sender")
  receiver <- ask[Person]("receiver")
} yield (sender, receiver)
```

This is fine, but if we wanted to ask the user for the name and age
separately we would have some duplication in our code. 

```scala
def senderAndReceiver2 = for {
  senderName   <- ask[String]("sender-name")
  senderAge    <- ask[Int]("sender-age")
  receiverName <- ask[String]("receiver-name")
  receiverAge  <- ask[Int]("receiver-age")
} yield (
  Person(senderName, senderAge),
  Person(receiverName, receiverAge)
)
```

Fortunately it is easy to create subjourneys and compose them into 
a larger journey. 

We can separate out the collection of the person record into a local
function -

```scala
def askPerson(id: String) = for {
  name <- ask[String](s"$id-name")
  age  <- ask[Int](s"$id-age")
} yield Person (name, age)

def senderAndReceiver3 = for {
  sender   <- askPerson("sender")
  receiver <- askPerson("receiver")
} yield (sender, receiver)
```


