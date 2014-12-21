package net.chrisloy.lisc

object BuiltIns {

  private type Args = List[Expression]

  val all: Map[Symbol, Eval] = Map(
    Symbol("=")  -> { implicit scope => _eq _ },
    Symbol("+")  -> { implicit scope => _add _ },
    Symbol("-")  -> { implicit scope => _minus _ },
    Symbol("<")  -> { implicit scope => _lt _ },
    Symbol("!")  -> { implicit scope => _not _ },
    Symbol("&&") -> { implicit scope => _and _}
  )

  def _eq(args: Args)(implicit scope: Scope): Value = args map (_.value) reduce (_ == _)

  def _add(args: Args)(implicit scope: Scope): Value = args match {
    case a if a.forall(_.isLong) => a.map(_.toLong).reduce(_ + _)
    case a => a map (_.toDouble) reduce (_ + _)
  }

  def _minus(args: Args)(implicit scope: Scope): Value = args match {
    case a if a.forall(_.isLong) => a.map(_.toLong).reduce(_ - _)
    case a => a map (_.toDouble) reduce (_ - _)
  }

  def _lt(args: Args)(implicit scope: Scope): Value = args match {
//   TODO make variadic
    case List(x, y) if x.isLong && y.isLong => x.toLong < y.toLong
    case List(x, y) => x.toDouble < y.toDouble
  }

  def _not(args: Args)(implicit scope: Scope): Value = args match {
    case List(x) => !x.toBoolean
  }

  def _and(args: Args)(implicit scope: Scope) = args.forall(_.toBoolean)
}
