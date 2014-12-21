package net.chrisloy.lisc

object SpecialForms {

  def apply(function: String): Option[Eval] = forms.lift(function)

  private val forms: PartialFunction[String, Eval] = {
    case "if" => {
      implicit scope => {
        case List(test, then, other) => if (test.toBoolean) then.value else other.value
      }
    }
    case "def" => {
      implicit scope => {
        case List(LLiteral(name), expr) => scope.Atoms.bind(name, expr)
      }
    }
    case "fn" => {
      implicit scope => {
        case List(LVector(args), expr) => scope.Functions.newFn(args, expr)
      }
    }
    case "defn" => {
      implicit scope => {
        case List(LLiteral(name), LVector(args), expr) => scope.Functions.bind(name, args, expr)
      }
    }
  }
}
