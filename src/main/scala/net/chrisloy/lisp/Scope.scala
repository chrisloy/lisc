package net.chrisloy.lisp

import scala.collection.mutable

class Scope {

  object Atoms {
    private val atoms = mutable.Map.empty[String, Expression]
    def apply(atom: String): Expression = atoms(atom)
  }

  object Functions {
    def apply(fn: String): Eval = {
      SpecialForms(fn) getOrElse BuiltIns(fn).get
    }
  }
}
