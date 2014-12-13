package net.chrisloy

package object lisp {

  sealed trait Expression { def value: Any }

  case class LLiteral(value: String) extends Expression
  case class LString(value: String) extends Expression
  case class LLong(value: Long) extends Expression
  case class LDouble(value: Double) extends Expression

  case class LList(members: List[Expression]) extends Expression {
    lazy val value = members match {
      case LLiteral(fn) :: args => defs(fn)(args)
      case _ => throw new Exception(s"Could not evaluate: $members")
    }
  }

  val addWithType: (Any, Any) => Any = {
    case (x: Long, y: Long) => x + y
    case (x: Long, y: Double) => x + y
    case (x: Double, y: Long) => x + y
    case (x: Double, y: Double) => x + y
    case (x, y) => throw new Exception(s"Don't know how to add $x and $y")
  }

  val defs = Map[String, List[Expression] => Any](
    "+" -> {
      _ map (_.value) reduce addWithType
    }
  )
}
