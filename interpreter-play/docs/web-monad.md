---
layout: docs
title: WebMonad
---

# How it works - the Web Monad

The uniform play interpreter is based off the observation that
interaction with a play-based web journey can be thought of as an
`Either[Result, A]` for a given `ask[A]` as long as `Either` is
right-associative monad.

Given an `ask[Address]` we would convert that to `Either[Result, Address]`

A `Right[Result, Address]` would represent that the user has supplied
a value which has passed validation, in this situation we can proceed
to the next step in the journey.

A `Left[Result, Address]` could represent a number of things, but for
simplicity we'll consider the main ones as -

1. No data submitted (render an empty form)
2. Data submitted, validation not passed (render a populated form with
   an error message)
3. Data submitted previously, form being revisited by the user (render a
   populated form)

When running a single `ask[A]` we get either a new page to be rendered (a
`Left(someResult)`) or we get a `Right[A]`. Because right-associative
`Either` is a `cats.Monad` that captures the error context we can compose
all our steps into one big `Either[Result, B]`.

Inside our controller we invoke our program with the play
interpreter. Along with the program we supply a _terminating fold_ of the
form `B ⇒ Future[Result]` which allows us to specify what should happen when
we get to the end of the journey along with what should be sent to the
user.

And this is the basis of how web journeys work in Uniform. In practice
we cannot encapsulate everything in an `Either[Result, ?]`, so the
actual `Webmonad` is closer to the following -

```
type WebInner[A] = RWST[Future, (JourneyConfig, List[String], Request[AnyContent]), Unit, (Path, DB), A]
type WebMonad[A] = EitherT[WebInner, Result, A]
```

Our steps must be within `Future` in order to support scenarios where
we need to call some remote process (for example to do an address
lookup).

The `JourneyConfig` reader is merely a placeholder for configuration options that
may be needed in the future.

The `List[String]` reader is used for the URL pieces, for example -

```
http://server.com/path-to-journey/one/two/three/ ⇒ List("one","two","three")
```

Normally the user will move through the journey naturally (there is a
redirect following a validated post), however this allows the user to
explicitly give a step (for example if they want to go back in the
journey and change an answer)[^1].

[^1]: Although the user can _request_ any page they like uniform will
    not allow the user to bypass validation or skip pages without
    answering questions.

The `Request` reader is used mainly for extracting the payload from a
POST request.

The `Path` state allows all pages to know the pages that have come
before them, and can be used to create breadcrumb hyperlinks on the
page.

`DB` is an alias for `Map[List[String],String]` and its usage in the
state is for serialising the data the user has been input. This is
handled by the `PersistenceEngine`.
