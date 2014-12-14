package net.chrisloy.lisp

import scala.collection.mutable

class Scope(_atoms: Map[String, Value] = Map.empty) {

  object Atoms {
    var atoms = _atoms
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
        params match {
          case Nil => { case Nil => body.value }
          case List(LLiteral(atom)) => { case List(a) => body.value(scope.plus(atom -> a.value))}
        }
      }
    }
  }

  def plus(tup: (String, Value)*): Scope = {
    new Scope(Atoms.atoms ++ Map(tup: _*))
  }
}
