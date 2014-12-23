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

  def bind(symbol: Symbol, expr: Any) = Values += symbol -> eval(expr)

  def bindFn(symbol: Symbol, params: List[Symbol], body: Any): Value = {
    Values += symbol -> newFn(params, body)
  }

  def eval(x: Any): Value = x match {
    case symbol: Symbol if Values.contains(symbol) => Values(symbol)
    case value @ (_: String | _: Long | _: Double | _: Boolean) => value
    case LVector(xs) => xs map eval
    case LList(xs) => SpecialForms(xs)(this) getOrElse this(xs)
    case Program(xs) => (xs map eval).head
  }

  // TODO better names? not here? as[T] ?
  def isSymbol(x: Any) = x.isInstanceOf[Symbol]
  def toSymbol(x: Any) = x.asInstanceOf[Symbol]
  def toBoolean(x: Any) = eval(x).asInstanceOf[Boolean]
  def isLong(x: Any) = eval(x).isInstanceOf[Long]
  def toLong(x: Any) = eval(x).asInstanceOf[Long]
  def isDouble(x: Any) = eval(x).isInstanceOf[Double]
  def toDouble(x: Any) = {
    if (isDouble(x)) eval(x).asInstanceOf[Double] else toLong(x).toDouble
  }


  def apply(xs: List[Any]): Value = {
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

  def newFn(params: List[Symbol], body: Any): Eval = {
    implicit scope => { args =>
      scope.plus(params.zip(args.map(scope.eval)): _*).eval(body)
    }
  }

  def plus(tup: (Symbol, Value)*) = Scope(Values ++ Map(tup: _*))
}
