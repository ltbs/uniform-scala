---
layout: docs
title: Internationalisation
---

# Internationalisation

Messages are provided as a `UniformMessages[Html]` object, where
`Html` is the data-type that composes the response (probably
`play.twirl.api.Html` if you're using twirl). 

It is possible to convert the play messages into `UniformMessages`
like so - 

```tut:invisible
import ltbs.uniform._, interpreters.playframework._
import play.api.mvc._
import play.twirl.api._
import cats.implicits._

def messagesApi: play.api.i18n.MessagesApi = ???
```

```tut:silent
def messages(request: Request[AnyContent]): UniformMessages[Html] =
  messagesApi.preferred(request).convertMessages.map{HtmlFormat.escape}
```

Under this configuration an exception will be thrown if a non-existant
key is asked for. `UniformMessage` is a `Monoid`, and as such can be
combined messages easily - the left-hand source will be consulted
first and if a message is not found the right-hand instance will be
asked. If we want a behaviour similar to play where the key itself is
returned if there is no message defined we can use the
`UniformMessages.echo` provider as a fallback - 

```tut:silent
  def messages(request: Request[AnyContent]): UniformMessages[Html] = {
    messagesApi.preferred(request).convertMessages() |+|
    UniformMessages.echo 
  } map ( HtmlFormat.escape )
```

A nice path for development is to use the play converted messages
combined with `UniformMessages.bestGuess` as a fallback. You may want
to run with `UniformMessages.attentionSeeker` if you are a content
author in order to display all optional content on the page. 
