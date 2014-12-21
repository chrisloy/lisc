LISc
====
_List Interpretation in Scala_

Overview
---
This is a toy LISP interpreter written in pure Scala. It is still early days and plenty of
language features are missing, but the code is in at least runnable, with a very basic REPL.

Usage
---
Fire up SBT, hit run, and you'll get a basic REPL:

```clj
$ sbt run

...

Welcome to LISc!
This is a toy Lisp interpreter, written in Scala.
Type :q to leave
    
lisc> (def x 4)
==> ()
lisc> (defn addX [y] (+ x y)
==> ()
lisc> (addX 6)
==> 10
```

Type ```:q``` to exit the REPL.
