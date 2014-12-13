package net.chrisloy

package object lisp {

  type Eval = List[Expression] => Any

  sealed trait Expression {
    def value: Any
    def toBoolean = value.asInstanceOf[Boolean]
  }

  case class LString(value: String) extends Expression
  case class LLong(value: Long) extends Expression
  case class LDouble(value: Double) extends Expression
  case class LBoolean(value: Boolean) extends Expression

  case class LLiteral(atom: String)(implicit scope: Scope) extends Expression {
    def value = scope.Atoms(atom)
  }

  case class LList(members: List[Expression])(implicit scope: Scope) extends Expression {
    lazy val value = members match {
      case LLiteral(fn) :: args => scope.Functions(fn)(args)
      case _ => throw new Exception(s"Could not evaluate: $members")
    }
  }
}
