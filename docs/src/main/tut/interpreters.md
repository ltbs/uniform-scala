---
layout: docs
title: Interpreters
position: 10
---

# Interpreters

A uniform program is processed by one or more interpreters to give a
useful output.

## Currently working 

1. A simple [Command-Line Interpreter](interpreter-cli.html)
2. A [Logic-table interpreter](interpreter-logic-table.html) that is
   useful for unit-testing and early exploratory work.
3. An interpreter that produces [controllers for the Play
   Framework](interpreter-play.html)
4. A [static site interpreter](interpreter-static-site.html) that
   outputs the logic as Javascript. Useful for producing design
   prototypes. 

## WIP 

1. A [selenium interpreter](interpreter-selenium.html), designed to
   be used in conjunction with either the play interpreter or the
   static site interpreter. For production of acceptance tests/taking
   screenshots of the user journey for given sets of input.

## Concepts (not started)

1. A [http4js](https://github.com/http4s/http4s) interpreter
2. A direct to [Akka HTTP](https://akka.io/) interpreter
