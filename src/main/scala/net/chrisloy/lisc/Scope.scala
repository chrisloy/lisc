package net.chrisloy.lisc

object Scope {
  def apply(): Scope = {
    val scope = Scope(BuiltIns.all)
    StandardLibrary.loadInto(scope)(new Parser)
    scope
  }
}

case class Scope(values: Map[Symbol, Value]) {

  private var Values = values

  def bind(symbol: Symbol, expr: Expression) = Values += symbol -> expr.value(this)

  def bindFn(symbol: Symbol, params: List[Symbol], body: Expression): Value = {
    Values += symbol -> newFn(params, body)
  }

  def eval(x: Expression): Value = {

    implicit val scope = this

    x match {
      case symbol: Symbol if Values.contains(symbol) => Values(symbol)
      case LVector(xs) => xs.map(_.value)
      case LList(xs) => SpecialForms(xs) getOrElse this(xs)
    }
  }

  def apply(xs: List[Expression]): Value = {
    xs.head match {
      case sym: Symbol => Values(sym) match {
        case fn: Eval => fn(this)(xs.tail)
      }
      case other => eval(other) match {
        case expr: Expression => apply(expr :: xs.tail)
        case fn: Eval => fn(this)(xs.tail)
      }
    }
  }

  def newFn(params: List[Symbol], body: Expression): Eval = {
    implicit scope => { args =>
      body.value(scope.plus(params.zip(args.map(_.value)): _*))
    }
  }

  def plus(tup: (Symbol, Value)*) = Scope(Values ++ Map(tup: _*))
}
