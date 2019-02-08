---
layout: docs
title: Setup
position: 1
---

# Installation - core uniform only

If you just want to write an abstract program in uniform and either
you will be creating your own interpreter or you want to release the
program in a separate jar file to where it will be executed then you
only need the core dependency in your `build.sbt` -

```
libraryDependencies += "com.luketebbs.uniform" %% "core" % "{{ site.last-stable-version }}"
```

Or if you're using ScalaJS - 

```
libraryDependencies += "com.luketebbs.uniform" %%% "core" % "{{ site.last-stable-version }}"
```

You can save yourself a lot of pain by using the kind projector compiler plugin - 

```
addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.9.3")
```

You will also need partial unification enabled - 

```
scalacOptions += "-Ypartial-unification"
```

# Adding an interpreter

A program by itself is not much use without an interpreter to execute
it. Uniform also provides several ready-to-use interpreters
out-of-the-box for you to experiment with - 

```
libraryDependencies += "com.luketebbs.uniform" %% "interpreter-play26" % "{{ site.last-stable-version }}"
```

