---
layout: docs
title: Memoisation
---

# Remote Calls in Play

Care must be taken when hooking remote calls into a journey to avoid
repeatedly hitting a server. For example consider this journey - 

```scala mdoc:silent
import ltbs.uniform._
import cats.implicits._
import scala.language.higherKinds

case class User (
	forename: String, 
	surname: String, 
	age: Int
)

trait Server[F[_]] {
    def userLookup(userName: String): F[User]
	def isAllowed(user: User): F[Boolean]
}

case class SensitiveData(value: String)

type TellTypes = SensitiveData :: NilTypes
type AskTypes = String :: NilTypes

def exampleJourney[F[_] : cats.Monad](
  interpreter: Language[F, TellTypes, AskTypes], 
  server: Server[F]
): F[String] = {
  import interpreter._

  for {
    user <- ask[String]("username") >>= server.userLookup
	_ <- (
	  tell[SensitiveData]("s1", SensitiveData("s1")), 
	  tell[SensitiveData]("s2", SensitiveData("s2")), 
	  tell[SensitiveData]("s3", SensitiveData("s3")), 
	  tell[SensitiveData]("s4", SensitiveData("s4"))
    ).tupled when server.isAllowed(user)
  } yield (user.surname)
}
```

This journey will ask the user for their username, look up their
account from `userLookup`. It will then show the user 4 items of
`SensitiveData` but only if they are permitted according to `isAllowed`.

Lets assume we have a `Server[Future]` instance that calls our server
and returns a response. 
We now need to implement a `Server[WebMonad]` so we can interleave
these calls into our journey. We might do this as follows - 

```scala mdoc:silent
import ltbs.uniform.common.web._
import ltbs.uniform.interpreters.playframework._
import play.twirl.api.Html
import concurrent._

case class ServerWrapper(
  inner: Server[Future]
)(implicit codec: Codec[User]) extends Server[WebMonad[?, Html]] {
  val adapter = FutureAdapter[Html]

  def userLookup(userName: String): WebMonad[User, Html] = 
    adapter.rerunOnPriorStateChange("userLookup")(
	  inner.userLookup(userName)
	)
    
  def isAllowed(user: User): WebMonad[Boolean, Html] =
    adapter.alwaysRerun(inner.isAllowed(user))
}
```

`FutureAdapter` provides access to natural transformations of the type
`Future ~> WebMonad`, but how you want the logic to execute is
probably contextual. 

Lets assume we don't want to overload the server
by lots of needless calls to `userLookup`, but if the user goes back
to the 'username' step and changes their answer we then want to
abandon the cached `User` and call the server again. For this purpose
the `rerunOnPriorStateChange` method gives us what we need - it takes
a checksum of the state of the previous pages and will only call the
inner method again if the checksum changes. 

However it is possible that an administrator revokes access to a user
after they have gone past the 'username' page, in which case caching
the result would be a bad thing. For this purpose we may decide we're
happy to trade a bit more load to the server in return for better
security. In this case using `alwaysRerun` would cause the user to be
unable to advance from 's1' to 's2' (for example) even if they had
access at the start of the journey because `isAllowed` runs again for
each step after its position in the journey. 

