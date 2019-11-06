---
layout: docs
title: Play Framework Interpreter
---

# Play framework interpreter

The play framework interpreter takes a uniform journey and converts it
into a series of web pages.

It can be used in conjunction with Play 2.5, 2.6 or 2.7, and it can be
retrofitted into an existing play application. As the web application
developer you are free to mix and match conventional journeys with
uniform journeys.

# Installation/Integration

In addition to the normal setup you will need to import the play interpreter. 

```
libraryDependencies += "com.luketebbs.uniform" %% "interpreter-play26" % "{{ site.last-stable-version }}"
```

# Composition/Separation

The simplest setup is just to define your journey inside the same
project as the controllers themselves, however it is good practice to
separate concerns and keep the journey separate from the
implementation. For example you could have a multi-project build
structured as follows -

```
- my-project
  - build.sbt
  - journey
    - src/main/scala
      - Journey.scala
    - src/test/scala
      - LogicTableBasedTests.scala (optional)
  - play-implementation
    - app/controllers
      - MyController.scala
  - other-subproject (optional)
```

Under this setup the `journey` sub-project would be set up to use
uniform core, whereas the `play-implementation` sub-project would
be a play application that has `dependsOn(journey)` in its
config. This means you are not tied into the play interpreter and you
could build parallel a play implementation in parallel with one or
more other implementations. 


