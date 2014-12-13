package net.chrisloy

package object lisp {

  type Eval = Scope => List[Expression] => Any

  sealed trait Expression {
    def value(implicit scope: Scope): Any
    def toBoolean(implicit scope: Scope) = value.asInstanceOf[Boolean]
  }

  abstract class Value[T](v: T) extends Expression {
    def value(implicit scope: Scope): T = v
  }

  case class LString(value: String) extends Value(value)
  case class LLong(value: Long) extends Value(value)
  case class LDouble(value: Double) extends Value(value)
  case class LBoolean(value: Boolean) extends Value(value)

  case class LLiteral(atom: String) extends Expression {
    def value(implicit scope: Scope) = scope.Atoms(atom)
  }

  case class LList(members: List[Expression]) extends Expression {
    def value(implicit scope: Scope) = members match {
      case LLiteral(fn) :: args => scope.Functions(fn)(scope)(args)
      case _ => throw new Exception(s"Could not evaluate: $members")
    }
  }
}
