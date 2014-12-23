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

  def bind(symbol: Symbol, expr: Expression) = Values += symbol -> eval(expr)

  def bindFn(symbol: Symbol, params: List[Symbol], body: Expression): Value = {
    Values += symbol -> newFn(params, body)
  }

  def eval(x: Expression): Value = {

    implicit val scope = this

    x match {
      case symbol: Symbol if Values.contains(symbol) => Values(symbol)
      case LString(value) => value
      case LLong(value) => value
      case LDouble(value) => value
      case LBoolean(value) => value
      case LVector(xs) => xs map eval
      case LList(xs) => SpecialForms(xs) getOrElse this(xs)
      case Program(xs) => (xs map eval).head
    }
  }

  // TODO better names? not here? as[T] ?
  def isSymbol(x: Expression) = x.isInstanceOf[Symbol]
  def toSymbol(x: Expression) = x.asInstanceOf[Symbol]
  def toBoolean(x: Expression) = eval(x).asInstanceOf[Boolean]
  def isLong(x: Expression) = eval(x).isInstanceOf[Long]
  def toLong(x: Expression) = eval(x).asInstanceOf[Long]
  def isDouble(x: Expression) = eval(x).isInstanceOf[Double]
  def toDouble(x: Expression) = {
    if (isDouble(x)) eval(x).asInstanceOf[Double] else toLong(x).toDouble
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
      scope.plus(params.zip(args.map(scope.eval)): _*).eval(body)
    }
  }

  def plus(tup: (Symbol, Value)*) = Scope(Values ++ Map(tup: _*))
}
