package net.chrisloy.lisc

import scala.util.{Failure, Success, Try}

object Main extends App {

  implicit val scope = Scope()

  val parse = new Parser
  val prompt = "lisc> "

  println(
    """
      |Welcome to LISc!
      |This is a toy Lisp interpreter, written in Scala.
      |Type :q to leave
    """.stripMargin)

  print(prompt)

  var commands: List[String] = Nil

  for (ln <- io.Source.stdin.getLines().takeWhile(_ != ":q")) {
    if (ln.nonEmpty) {
      Try(scope.eval(parse(ln))) match {

        case Failure(ex) =>
          println(s"Parse error: ${ex.getMessage}")
          ex.printStackTrace()

        case Success(eval) =>
          commands ::= ln
          println(s"==> $eval")
      }
    }
    print(prompt)
  }

  println("Done. This is what you wrote:")

  commands.reverse foreach println
}

