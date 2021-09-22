---
layout: docs
title: Telling
---

> **A `tell` is a step in a Uniform journey that passes information to the user.**

> **An `end` is a step that passes information to the user, but which they cannot continue beyond.**

# tell


A call to `tell` represents passing some typed data to the user. The
specifics of exactly how this should be done depends upon the
interpreter that is running the program and in particular how it
chooses to handle that datatype. 

```scala
// create some data
case class Story(title: String, author: String)
val davidCopperfield = Story("David Copperfield","Charles Dickens")

// construct a journey that presents the data to the user
val tellAStory = tell("story-telling", davidCopperfield)
```

`"story-telling"` in this case is the step
identifier - these are presented to the user but the exact form will
depend upon the interpreter used. It may for example be used to
construct messages telling the user which question they are answering,
for forming URL's or for persistence of data.

Step identifiers should be unique to every step as otherwise the
interpreters can get confused as they do not know the current place in the
journey.

We can gain a bit more insight into the `tell` operation by examining
the type signature - 

```scala
val anInspectorCalls = Story("An Inspector Calls","J B Priestly")
val tellAnotherStory: Uniform[Needs.Interact[Story, Unit], Story, Unit] = 
  tell("an-inspector-calls", anInspectorCalls)
```

The first type parameter `Needs.Interact[Story, Unit]` is used for
tracking which types the interpreter needs to support, is
discussed later on and can be ignored for the moment. 

The second type parameter `Story` is the _input_ parameter - that is
being expressed to the user. 

The third and final type parameter `Unit` is the type we are asking
from the user (we're not asking them for anything).

# end 

Closely related to `tell` is `end`, the main difference being that
while the third type parameter (requested data type) of `tell` is
always `Unit` the return type for `end` is `Nothing`.

`Unit` has a cardinality of 1 - there is only one value the user may
return from a `tell` - acknowledgement and a desire to continue. But
`Nothing` has a cardinality of zero - there is no response the user
can give at all.

As a result a journey can continue after a `tell`, but
not after an `end`.

`end`'s are used at terminating points in a user interaction - for
example a website may collect some information before processing an
application, but it may be that the user has already applied. In this
case the interaction designer may decide to use an `end` page to
inform the user of this fact.

