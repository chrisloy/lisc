package net.chrisloy.lisc

object Scope {
  def apply(): Scope = Scope(Map.empty, BuiltIns.all) // TODO move std lib here
}

case class Scope(values: Map[Symbol, Value], functions: Map[Symbol, Eval]) {

  var Values = values
  var Functions = functions

  def bind(symbol: Symbol, expr: Expression) = Values += symbol -> expr.value(this)

  def bindFn(symbol: Symbol, params: List[Expression], body: Expression): Value = {
    Functions += symbol -> newFn(params, body)
  }

  def eval(x: Expression): Value = {

    implicit val scope = this

    x match {
      case symbol: Symbol if Values.contains(symbol) => Values(symbol)
      case symbol: Symbol if Functions.contains(symbol) => Functions(symbol)
      case LVector(xs) => xs.map(_.value)
      case LList(xs) => SpecialForms(xs) getOrElse this(xs)
    }
  }

  def apply(xs: List[Expression]): Value = {
    xs.head match {
      case sym: Symbol => Functions(sym)(this)(xs.tail)
      case other => eval(other) match {
        case expr: Expression => apply(expr :: xs.tail)
        case fn: Eval => fn(this)(xs.tail)
      }
    }
  }

  def newFn(params: List[Expression], body: Expression): Eval = {
    implicit scope => {
      params match {
        case Nil => { case Nil => body.value }
        case List(sym: Symbol) => { case List(a) => body.value(scope.plus(sym -> a.value))}
      }
    }
  }

  def plus(tup: (Symbol, Value)*) = Scope(Values ++ Map(tup: _*), Functions)
}
