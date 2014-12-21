package net.chrisloy

package object lisc {

  case class Symbol(name: String) extends Expression {
    def value(implicit scope: Scope) = scope.eval(this)
  }

  type Value = Any
  type Eval = Scope => List[Expression] => Value

  sealed trait Expression {
    def value(implicit scope: Scope): Any
    def isSymbol(implicit scope: Scope) = this.isInstanceOf[Symbol]
    def toSymbol(implicit scope: Scope) = this.asInstanceOf[Symbol]
    def toBoolean(implicit scope: Scope) = value.asInstanceOf[Boolean]
    def isLong(implicit scope: Scope) = value.isInstanceOf[Long]
    def toLong(implicit scope: Scope) = value.asInstanceOf[Long]
    def isDouble(implicit scope: Scope) = value.isInstanceOf[Double]
    def toDouble(implicit scope: Scope) = {
      if (isDouble) value.asInstanceOf[Double] else toLong.toDouble
    }
  }

  abstract class WithValue[T](v: T) extends Expression {
    def value(implicit scope: Scope): T = v
  }

  case class LString(value: String) extends WithValue(value)
  case class LLong(value: Long) extends WithValue(value)
  case class LDouble(value: Double) extends WithValue(value)
  case class LBoolean(value: Boolean) extends WithValue(value)

  case class LList(members: List[Expression]) extends Expression {
    def value(implicit scope: Scope) = scope.eval(this)
  }

  case class LVector(members: List[Expression]) extends Expression {
    def value(implicit scope: Scope) = members.map(_.value)
  }

  case class Program(members: List[Expression]) extends Expression {
    def value(implicit scope: Scope) = members.map(_.value).last
  }
}
