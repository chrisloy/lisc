package net.chrisloy.lisp

object SpecialForms {

  def apply(function: String): Option[Eval] = forms.lift(function)

  private val forms: PartialFunction[String, Eval] = {
    case "if" => {
      case List(test, then, other) => if (test.toBoolean) then.value else other.value
    }
  }
}
