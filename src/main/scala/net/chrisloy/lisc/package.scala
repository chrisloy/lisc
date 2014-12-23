package net.chrisloy

package object lisc {

  case class Symbol(name: String) extends Expression

  type Value = Any
  type Eval = Scope => List[Expression] => Value

  sealed trait Expression {
    final def value(implicit scope: Scope): Any = scope.eval(this)
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

  case class LString(value: String) extends Expression
  case class LLong(value: Long) extends Expression
  case class LDouble(value: Double) extends Expression
  case class LBoolean(value: Boolean) extends Expression

  case class LList(members: List[Expression]) extends Expression
  case class LVector(members: List[Expression]) extends Expression
  case class Program(members: List[Expression]) extends Expression
}
