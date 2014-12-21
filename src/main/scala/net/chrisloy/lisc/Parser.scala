package net.chrisloy.lisc

import scala.util.parsing.combinator._

class Parser {

  private def stripQuotes(x: String) = x.substring(1, x.length - 1)

  val parser = new JavaTokenParsers {

    private def list: Parser[LList] =
      "(" ~> rep(exp) <~ ")" ^^ (x => LList(x))

    private def vector: Parser[LVector] =
      "[" ~> rep(exp) <~ "]" ^^ (x => LVector(x))

    private def string: Parser[LString] =
      stringLiteral ^^ stripQuotes ^^ LString

    private def symbol: Parser[Symbol] =
      """[^()\[\] ]+""".r ^^ (x => new Symbol(x))

    private def long: Parser[LLong] =
      wholeNumber ^^ (_.toLong) ^^ LLong

    private def double: Parser[LDouble] =
      ("""\-?\d+\.\d*([eE]\-?\d+)?""".r | """\-?\d+[eE]\-?\d+""".r) ^^ (_.toDouble) ^^ LDouble

    private def boolean: Parser[LBoolean] =
      "true"  ^^^ LBoolean(value = true) | "false" ^^^ LBoolean(value = false)

    private def exp: Parser[Expression] =
      boolean | double | long | string | list | vector | symbol

    def program: Parser[Program] = rep(exp) ^^ Program
  }

  def apply(input: String): Expression = parser.parseAll(parser.program, input).get
}
