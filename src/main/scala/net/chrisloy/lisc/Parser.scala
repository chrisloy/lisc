package net.chrisloy.lisc

import scala.util.parsing.combinator._

class Parser {

  private def stripQuotes(x: String) = x.substring(1, x.length - 1)

  val parser = new JavaTokenParsers {

    private def list: Parser[LList] =
      "(" ~> rep(exp) <~ ")" ^^ (x => LList(x))

    private def vector: Parser[LVector] =
      "[" ~> rep(exp) <~ "]" ^^ (x => LVector(x))

    private def string: Parser[String] =
      stringLiteral ^^ stripQuotes

    private def symbol: Parser[Symbol] =
      """[^()\[\] ]+""".r ^^ (x => new Symbol(x))

    private def long: Parser[Long] =
      wholeNumber ^^ (_.toLong)

    private def double: Parser[Double] =
      ("""\-?\d+\.\d*([eE]\-?\d+)?""".r | """\-?\d+[eE]\-?\d+""".r) ^^ (_.toDouble)

    private def boolean: Parser[Boolean] =
      "true"  ^^^ true | "false" ^^^ false

    private def exp: Parser[Any] =
      boolean | double | long | string | list | vector | symbol

    def program: Parser[Program] = rep(exp) ^^ Program
  }

  def apply(input: String): Expression = parser.parseAll(parser.program, input).get
}
