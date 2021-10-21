---
layout: docs
title: Alternative templating technologies
---

> **The play interpreter rendering can be easily swapped out and**
> **works as well with Scalatags as it does with twirl.**

# scalatags

Scalatags is an attractive alternative when doing web
development in scala, and it is easy to swap out the rendering to use
this instead of twirl. 

Firstly you will need to have scalatags as a library dependency. 

Secondly you will need to tell the Play Framework how to render the
`Tag` type - 


```scala 
package controllers

import play.api.http.{Writeable, ContentTypeOf, ContentTypes}
import play.api.mvc.Codec
import scalatags.Text.all._

trait ScalatagsSupport {

  implicit def contentTypeOfTag(implicit codec: Codec): ContentTypeOf[Tag] = {
    ContentTypeOf[Tag](Some(ContentTypes.HTML))
  }

  implicit def writeableOfTag(implicit codec: Codec): Writeable[Tag] = {
    Writeable(tag => codec.encode("<!DOCTYPE html>\n" + tag.render))
  }

}

object ScalatagsSupport extends ScalatagsSupport
```

Now all you need to do is to implement your controller to extend
`PlayInterpreter[Tag]` and `ScalatagsSupport` rather than `PlayInterpreter[Html]`.

# other technologies

As you can see `PlayInterpreter` can support any datatype for
rendering and as such it should be possible to plug in whichever
technology you prefer for rendering.

While it is possible to have two controllers use different rendering
technologies within the same project this is not advised as it
limits reuse of components - you will likely find yourself having to
write a `WebAsk[Tag, Int]` as well as a `WebAsk[Html, Int]`.
