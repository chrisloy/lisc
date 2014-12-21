package net.chrisloy.lisc

object Scope {
  def apply(): Scope = Scope(Map.empty, Map.empty) // TODO move std lib here
}

case class Scope(values: Map[Symbol, Value], functions: Map[Symbol, Eval]) {

  var Values = values
  var Functions = functions

  def bind(symbol: Symbol, expr: Expression)(implicit scope: Scope) = Values += symbol -> expr.value

  def bindFn(symbol: Symbol, params: List[Expression], body: Expression)(implicit scope: Scope): Value = {
    Functions += symbol -> newFn(params, body)
  }

  def eval(x: Expression): Value = {

    implicit val scope = this

    x match {
      case symbol: Symbol if Values.contains(symbol) => Values(symbol)
      case LVector(xs) => xs.map(_.value)
      case LList(xs) => SpecialForms(xs) orElse BuiltIns(xs) getOrElse this(xs)
    }
  }

  def apply(xs: List[Expression])(implicit scope: Scope): Value = {
    xs.head match {
      case sym: Symbol => Functions(sym)(scope)(xs.tail)
      case fn: Eval => fn(scope)(xs.tail)
    }
  }

  def newFn(params: List[Expression], body: Expression)(implicit scope: Scope): Eval = {
    implicit scope => {
      params match {
        case Nil => { case Nil => body.value }
        case List(sym: Symbol) => { case List(a) => body.value(scope.plus(sym -> a.value))}
      }
    }
  }

  def plus(tup: (Symbol, Value)*) = Scope(Values ++ Map(tup: _*), Functions)
}
