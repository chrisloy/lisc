package net.chrisloy.lisc

object BuiltIns {

  private type Args = List[Expression]

  val all: Map[Symbol, Eval] = Map(
    Symbol("=") -> { implicit scope => _eq _ },
    Symbol("+") -> { implicit scope => _add _ }
  )

  def _eq(args: Args)(implicit scope: Scope): Any = args map (_.value) reduce (_ == _)

  def _add(args: Args)(implicit scope: Scope): Any = args match {
    case a if a.forall(_.isLong) => a.map(_.toLong).reduce(_ + _)
    case a => a map (_.toDouble) reduce (_ + _)
  }
}
