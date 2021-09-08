---
layout: docs
title: Internationalisation
---

# Internationalisation

Messages are provided as a `UniformMessages[Html]` object, where
`Html` is the data-type that composes the response (such as
`play.twirl.api.Html` if you're using twirl with the Play Framework). 

Under this configuration an exception will be thrown if a non-existant
key is asked for. `UniformMessage` is a `Monoid`, and as such can be
combined messages easily - the left-hand source will be consulted
first and if a message is not found the right-hand instance will be
asked. If we the key itself to be returned if there is no message defined we can use the
`UniformMessages.echo` provider as a fallback - 

```scala
  def messages2(request: Request[AnyContent]): UniformMessages[Html] = {
    messagesApi.preferred(request).convertMessages() |+|
    UniformMessages.echo 
  } map ( HtmlFormat.escape )
```

A nice path for development is to use `UniformMessages.bestGuess` as a
fallback. You may want to run with `UniformMessages.attentionSeeker`
if you are a content author in order to display all optional content
on the page.
