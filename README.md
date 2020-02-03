# uniform-scala

Functional user journeys in Scala. Write your user-journeys once in an
extensible Scala
[eDSL](https://en.wikipedia.org/wiki/Domain-specific_language#Usage_patterns)
and have interpreters build for you -

1. A Web interface in [Play](https://www.playframework.com/)
2. A GUI interface
3. A CLI interface
4. A Static website
5. Logic tables 
6. Selenium Tests

See the [documentation](https://ltbs.github.io/uniform-scala/) to get started.

## Building 

It is necessary to build the play28 interpreter separately, for example if running 
`publishSigned` - 

```
sbt +publishSigned +interpreter-play28/publishSigned
```

Without doing this you will likely find SBT will attempt to change `crossScalaVersions` to 
include incompatible combinations of Play and Scala.
