package net.chrisloy.lisc

object SpecialForms {

  def apply(xs: List[Any])(scope: Scope): Option[Value] = xs match {
    case Symbol(x) :: args => forms(scope).lift(x, args)
    case _ => None
  }

  private def forms(scope: Scope): PartialFunction[(String, List[Any]), Value] = {

    import scope._

    {
      case ("if", List(test, then, other)) =>
        if (toBoolean(test)) eval(then) else scope.eval(other)

      case ("def", List(symbol, expr)) =>
        bind(toSymbol(symbol), expr)

      case ("fn", List(LVector(args), expr)) =>
        newFn(args map toSymbol , expr)

      case ("defn", List(symbol, LVector(args), expr)) =>
        bindFn(toSymbol(symbol), args map toSymbol, expr)
    }
  }
}
