package net.chrisloy.lisp

import scala.collection.mutable

class Scope {

  object Atoms {
    private val atoms = mutable.Map.empty[String, Any]
    def apply(atom: String): Any = atoms(atom)
    def bind(name: String, expr: Expression)(implicit scope: Scope) = atoms += name -> expr.value
  }

  object Functions {
    def apply(fn: String): Eval = {
      SpecialForms(fn) getOrElse BuiltIns(fn).get
    }
  }
}
