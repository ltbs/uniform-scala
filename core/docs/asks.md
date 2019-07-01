---
layout: docs
title: Asking
---

```tut:invisible
import ltbs.uniform._
import cats.implicits._
import scala.language.higherKinds
```

# asks

```
def ask[A](stepId: String): F[A]
```

Whereas a `tell` represents sending typed information to the user an
`ask` is the reverse - it represents prompting the user to supply some
data. 


