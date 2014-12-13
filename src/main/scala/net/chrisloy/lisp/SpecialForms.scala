package net.chrisloy.lisp

object SpecialForms {

  def apply(function: String): Option[Eval] = function match {

    case "if" => Some(
      args => if (args(0).toBoolean) args(1).value else args(2).value
    )

    case _ => None
  }
}
