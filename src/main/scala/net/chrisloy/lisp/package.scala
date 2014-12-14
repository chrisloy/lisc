package net.chrisloy

package object lisp {

  type Value = Any
  type Eval = Scope => List[Expression] => Value

  sealed trait Expression {
    def value(implicit scope: Scope): Any
    def toBoolean(implicit scope: Scope) = value.asInstanceOf[Boolean]
  }

  abstract class WithValue[T](v: T) extends Expression {
    def value(implicit scope: Scope): T = v
  }

  case class LString(value: String) extends WithValue(value)
  case class LLong(value: Long) extends WithValue(value)
  case class LDouble(value: Double) extends WithValue(value)
  case class LBoolean(value: Boolean) extends WithValue(value)

  case class LLiteral(atom: String) extends Expression {
    def value(implicit scope: Scope) = scope.Atoms(atom)
  }

  case class LList(members: List[Expression]) extends Expression {
    def value(implicit scope: Scope) = members match {
      case LLiteral(fn) :: args => scope.Functions(fn)(scope)(args)
      case (x: LList) :: args => x.value.asInstanceOf[Eval](scope)(args)
      case _ => throw new Exception(s"Could not evaluate: $members")
    }
  }

  case class LVector(members: List[Expression]) extends Expression {
    def value(implicit scope: Scope) = members.map(_.value)
  }
}
