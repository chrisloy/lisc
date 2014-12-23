LISc
====
_List Interpretation in Scala_

[![Build Status](https://travis-ci.org/chrisloy/lisc.png?branch=master)](https://travis-ci.org/chrisloy/lisc)

Overview
---
This is a toy LISP interpreter written in pure Scala. It is still early days and plenty of
language features are missing, but the code is at least runnable, with a very basic REPL.

Usage
---
Fire up SBT, hit run, and you'll get a basic REPL:

```clj
$ sbt run

...

Welcome to LISc!
This is a toy Lisp interpreter, written in Scala.
Type :q to leave
    
lisc> (defn fact [x] (if (<= x 1) 1 (* x (fact (- x 1)))))
==> ()
lisc> (fact 5)
==> 120
```

Type ```:q``` to exit the REPL.

Special Forms
---
Currently four, with naming inspiration nabbed from Clojure:

* ```(if test then else)``` - as you might expect
* ```(def name value)``` - binds a value to the result of the expression
* ```(fn [args] body)``` - defines a lambda function
* ```(defn name [args] body)``` - helper for binding a function (to be replaced by a macro once they're implemented)

