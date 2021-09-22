---
layout: docs
title: Telling
---

# tells

```
def tell[A](stepId: String, value: A): Uniform[Unit]
```

A call to `tell` represents passing some typed data to the user. The
specifics of exactly how this should be done depends upon the
interpreter that is running the program and in particular how it
chooses to handle that datatype. 

If we call `tell[A]("step-name", presentationData)` we will get an
`F[Unit]` emitted. `presentationData` must be of type `A` and `A` must
be in the types supported by the interpreter.

`"step-name"` in this case is the step
identifier - these are presented to the user but the exact form will
depend upon the interpreter used. It may for example be used to
construct messages telling the user which question they are answering,
for forming URL's or for persistence of data.

Step identifiers should be unique not only to all the `tell`s in the
journey but across all the `ask`s and `interact`s too as otherwise the
interpreters can get confused as they do not know the current place in the
journey.

