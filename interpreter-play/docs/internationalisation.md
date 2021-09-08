---
layout: docs
title: Internationalisation
---

# Internationalisation

It is possible to convert the play messages into `UniformMessages`
like so - 

```scala
import ltbs.uniform._, interpreters.playframework._
import play.api.mvc._
import play.twirl.api._
import cats.implicits._

def messagesApi: play.api.i18n.MessagesApi = ???
```

```scala
def messages(request: Request[AnyContent]): UniformMessages[Html] =
  messagesApi.preferred(request).convertMessages.map{HtmlFormat.escape}
```
