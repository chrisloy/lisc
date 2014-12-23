package net.chrisloy.lisc

object BuiltIns {

  private type Args = List[Any]

  val all: Map[Symbol, Eval] = Map(
    Symbol("=")  -> { implicit scope => _eq _ },
    Symbol("+")  -> { implicit scope => _add _ },
    Symbol("-")  -> { implicit scope => _minus _ },
    Symbol("<")  -> { implicit scope => _lt _ },
    Symbol("!")  -> { implicit scope => _not _ },
    Symbol("&&") -> { implicit scope => _and _}
  )

  def _eq(args: Args)(implicit scope: Scope): Value = args map scope.eval reduce (_ == _)

  def _add(args: Args)(implicit scope: Scope): Value = args match {
    case a if a.forall(scope.isLong) => a.map(scope.toLong).reduce(_ + _)
    case a => a.map(scope.toDouble) reduce (_ + _)
  }

  def _minus(args: Args)(implicit scope: Scope): Value = args match {
    case a if a.forall(scope.isLong) => a.map(scope.toLong).reduce(_ - _)
    case a => a.map(scope.toDouble) reduce (_ - _)
  }

  def _lt(args: Args)(implicit scope: Scope): Value = args match {
//   TODO make variadic
    case List(x, y) if scope.isLong(x) && scope.isLong(y) => scope.toLong(x) < scope.toLong(y)
    case List(x, y) => scope.toDouble(x) < scope.toDouble(y)
  }

  def _not(args: Args)(implicit scope: Scope): Value = args match {
    case List(x) => !scope.toBoolean(x)
  }

  def _and(args: Args)(implicit scope: Scope) = args.forall(scope.toBoolean)
}
