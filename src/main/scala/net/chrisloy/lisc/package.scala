package net.chrisloy

package object lisc {

  type Symbol = String
  type Value = Any
  type Eval = Scope => List[Expression] => Value

  sealed trait Expression {
    def value(implicit scope: Scope): Any
    def toSymbol(implicit scope: Scope) = value.asInstanceOf[Symbol]
    def toBoolean(implicit scope: Scope) = value.asInstanceOf[Boolean]
    def isLong(implicit scope: Scope) = value.isInstanceOf[Long]
    def toLong(implicit scope: Scope) = value.asInstanceOf[Long]
    def isDouble(implicit scope: Scope) = value.isInstanceOf[Long]
    def toDouble(implicit scope: Scope) = value.asInstanceOf[Double]
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
    def value(implicit scope: Scope) = scope.eval(members)
  }

  case class LVector(members: List[Expression]) extends Expression {
    def value(implicit scope: Scope) = members.map(_.value)
  }
}
