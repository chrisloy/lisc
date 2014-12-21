package net.chrisloy.lisc

object BuiltIns {

//  def apply(xs: List[Expression])(implicit scope: Scope): Option[Value] = xs match {
//    case Symbol(name) :: args => functions.lift(name, args)
//    case _ => None
//  }
//
//  private def functions(implicit scope: Scope): PartialFunction[(String, List[Expression]), Value] = {
//    case ("+", args) if args.forall(_.isLong) => args.map(_.toLong).reduce(_ + _)
//    case ("+", args) => args map (_.toDouble) reduce (_ + _)
//    case ("=", args) => args map (_.value) reduce (_ == _)
//  }

  val all: Map[Symbol, Eval] = Map(
    Symbol("=") -> { implicit scope => _ map (_.value) reduce (_ == _) },
    Symbol("+") -> { implicit scope => {
      case args if args.forall(_.isLong) => args.map(_.toLong).reduce(_ + _)
      case args => args map (_.toDouble) reduce (_ + _)
    }}
  )
}
