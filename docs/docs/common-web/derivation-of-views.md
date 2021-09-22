---
layout: docs
title: Derivation of views
---

> **Instances of `WebAsk` for compound types can be derived from instances 
> of its components**

# derivation of views

One of the major strengths of using Uniform for web development is
that the generic web interpreter can automatically define how a form
should be visually represented and encoded and persisted for you.

If you define a `WebAsk[Html, A]` and a `WebAsk[Html, B]` along with a
few simple rules to define composition you can have for free
`WebAsk[Html,(A,B)]`, `WebAsk[Html,Either[A,B]]`, or
`WebAsk[Html,C]` where `C` is any combination of `A` and `B`.

```scala
import ltbs.uniform._, common.web._

// we'll use this as a simple representation
// of HTML, in practice you'd use something more 
// substantial like scalatags `Tag` or twirl `Html`
case class Html(value: String)

// Implement InferWebAsk
object MyWidgets extends InferWebAsk[Html] { 

  // define how to render a product
  // (f.e. using a fieldset)
  def renderAnd(
    pageKey: List[String],
    fieldKey: List[String],
    tell: Option[Html],
    breadcrumbs: Breadcrumbs,
    data: Input,
    errors: ErrorTree,
    messages: UniformMessages[Html],
    members: Seq[(String, Html)]
  ): Html = ???

  // define how to render a coproduct
  // (f.e. using radio buttons)
  def renderOr(
    pageKey: List[String],
    fieldKey: List[String],
    tell: Option[Html],
    breadcrumbs: Breadcrumbs,
    data: Input,
    errors: ErrorTree,
    messages: UniformMessages[Html],
    alternatives: Seq[(String, Option[Html])],
    selected: Option[String]
  ): Html = ???
  
  // define a few simple primative types 
  implicit def askInt : WebAsk[Html, Int] = ???
  implicit def askString : WebAsk[Html, String] = ???
  implicit def askBool : WebAsk[Html, Boolean] = ???

}
```

Once you have an implementation of `InferWebAsk` you can use it to
provide derived types - 

```scala
import MyWidgets._
implicit def askTuple: WebAsk[Html, (Int, Boolean)] = implicitly

sealed trait One
case class Two(a: String, b: Boolean)
case class Three(a: String, c: Either[Boolean, Int])

implicit def askHierarchy: WebAsk[Html, One] = implicitly
```

What this means in effect is that in return for a little bit of
up-front work in defining the way fields should be composed together
you gain the ability to manipulate the data structures at will and
have the interface adapt itself. 

This is valuable at all points through the product lifecycle but
particularly so during prototyping.
