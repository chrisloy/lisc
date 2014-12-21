package net.chrisloy.lisc

object BuiltIns {

  def apply(atom: String): Option[Eval] = functions.lift(atom)

  private val functions: PartialFunction[String, Eval] = {
    case "+" => implicit scope => _ map (_.value) reduce addWithType
  }

  // TODO
  private lazy val addWithType: (Any, Any) => Value = {
    case (x: Long, y: Long) => x + y
    case (x: Long, y: Double) => x + y
    case (x: Double, y: Long) => x + y
    case (x: Double, y: Double) => x + y
    case (x, y) => throw new Exception(s"Don't know how to add $x and $y")
  }
}
