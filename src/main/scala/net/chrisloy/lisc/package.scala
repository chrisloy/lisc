package net.chrisloy

package object lisc {

  case class Symbol(name: String) extends Expression

  type Value = Any
  type Eval = Scope => List[Any] => Value

  sealed trait Expression

  case class LList(members: List[Any]) extends Expression
  case class LVector(members: List[Any]) extends Expression
  case class Program(members: List[Any]) extends Expression
}
