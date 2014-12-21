package net.chrisloy.lisc

object Scope {
  def apply(): Scope = Scope(Map.empty, Map.empty) // TODO move std lib here
}

case class Scope(values: Map[String, Value], functions: Map[Symbol, Eval]) {

  var Values = values
  var Functions = functions

  def bind(symbol: Symbol, expr: Expression)(implicit scope: Scope) = Values += symbol -> expr.value

  def bindFn(name: String, params: List[Expression], body: Expression)(implicit scope: Scope): Value = {
    Functions += name -> newFn(params, body)
  }

  implicit val scope = this

  def eval(xs: List[Expression]): Value = {
    SpecialForms(xs) orElse BuiltIns(xs) getOrElse this(xs)
  }

  def apply(symbol: Symbol): Any = values(symbol)


  def apply(xs: List[Expression])(implicit scope: Scope): Value = {
    xs.head match {
      case fn: Eval => fn(scope)(xs.tail)
      case LLiteral(sym) => Functions(sym)(scope)(xs.tail)
    }
  }

  def newFn(params: List[Expression], body: Expression)(implicit scope: Scope): Eval = {
    implicit scope => {
      params match {
        case Nil => { case Nil => body.value }
        case List(LLiteral(atom)) => { case List(a) => body.value(scope.plus(atom -> a.value))}
      }
    }
  }

  def plus(tup: (String, Value)*) = Scope(Values ++ Map(tup: _*), Functions)
}
