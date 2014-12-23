package net.chrisloy

package object lisc {

  case class Symbol(name: String) extends Expression

  type Value = Any
  type Eval = Scope => List[Expression] => Value

  sealed trait Expression

  case class LString(value: String) extends Expression
  case class LLong(value: Long) extends Expression
  case class LDouble(value: Double) extends Expression
  case class LBoolean(value: Boolean) extends Expression

  case class LList(members: List[Expression]) extends Expression
  case class LVector(members: List[Expression]) extends Expression
  case class Program(members: List[Expression]) extends Expression
}
