package net.chrisloy.lisc

object Main extends App {

  implicit val scope = new Scope

  val parse = new Parser
  val prompt = "lisc> "

  println(
    """
      |Welcome to LISc!
      |This is a toy Lisp interpreter, written in Scala.
      |Type :q to leave
    """.stripMargin)

  print(prompt)

  for (ln <- io.Source.stdin.getLines().takeWhile(_ != ":q")) {
    if (ln.nonEmpty) {
      val eval = parse(ln).value
      println(s"==> $eval")
    }
    print(prompt)
  }
}

