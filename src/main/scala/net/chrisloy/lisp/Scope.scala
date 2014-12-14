package net.chrisloy.lisp

import scala.collection.mutable

class Scope {

  object Atoms {
    private val atoms = mutable.Map.empty[String, Any]
    def apply(atom: String): Any = atoms(atom)
    def bind(name: String, expr: Expression)(implicit scope: Scope) = atoms += name -> expr.value
  }

  object Functions {
    private val userDefined = mutable.Map.empty[String, Eval]

    def bind(name: String, params: List[Expression], body: Expression)(implicit scope: Scope): Value = {
      userDefined += name -> newFn(params, body)
    }

    def apply(fn: String): Eval = {
      SpecialForms(fn) orElse BuiltIns(fn) getOrElse userDefined(fn)
    }

    def newFn(params: List[Expression], body: Expression)(implicit scope: Scope): Eval = {
      implicit scope => {
        params.size match {
          case 0 => { case Nil => body.value }
        }
      }
    }
  }
}
