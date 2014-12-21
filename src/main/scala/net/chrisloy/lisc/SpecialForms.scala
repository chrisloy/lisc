package net.chrisloy.lisc

object SpecialForms {

  def apply(xs: List[Expression])(implicit scope: Scope): Option[Value] = xs match {
    case Symbol(x) :: args => forms.lift(x, args)
    case _ => None
  }

  private def forms(implicit scope: Scope): PartialFunction[(String, List[Expression]), Value] = {
    case ("if", List(test, then, other)) => if (test.toBoolean) then.value else other.value
    case ("def", List(symbol, expr)) => scope.bind(symbol.toSymbol, expr)
    case ("fn", List(LVector(args), expr)) => scope.newFn(args, expr)
    case ("defn", List(symbol, LVector(args), expr)) => scope.bindFn(symbol.toSymbol, args, expr)
  }
}
