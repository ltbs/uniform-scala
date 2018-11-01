---
layout: home

---

# Installation - core uniform only

If you just want to write an abstract program in uniform and either
you will be creating your own interpreter or you want to release the
program in a separate jar file to where it will be executed then you
only need the core dependency in your `build.sbt` -

```
libraryDependencies += "com.luketebbs.uniform" % %% "core" % "VERSION"
```

# Adding an interpreter

A program by itself is not much use without an interpreter to execute
it. Uniform also provides several ready-to-use interpreters
out-of-the-box for you to experiment with - 

```
libraryDependencies += "com.luketebbs.uniform" %% "interpreter-play26" % "VERSION"
```

