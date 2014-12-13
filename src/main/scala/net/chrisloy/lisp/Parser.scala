package net.chrisloy.lisp

import scala.util.parsing.combinator._

class Parser(implicit scope: Scope) {

  private def stripQuotes(x: String) = x.substring(1, x.length - 1)

  val parser = new JavaTokenParsers {

    private def list: Parser[LList] =
      "(" ~> rep(exp) <~ ")" ^^ (x => LList(x))

    private def string: Parser[LString] =
      stringLiteral ^^ stripQuotes ^^ LString

    private def literal: Parser[LLiteral] =
      """[^() ]+""".r ^^ (x => LLiteral(x))

    private def long: Parser[LLong] =
      wholeNumber ^^ (_.toLong) ^^ LLong

    private def double: Parser[LDouble] =
      ("""\-?\d+\.\d*([eE]\-?\d+)?""".r | """\-?\d+[eE]\-?\d+""".r) ^^ (_.toDouble) ^^ LDouble

    private def boolean: Parser[LBoolean] =
      "true"  ^^^ LBoolean(value = true) | "false" ^^^ LBoolean(value = false)

    val exp: Parser[Expression] =
      boolean | double | long | string | list | literal
  }

  def apply(input: String): Expression = parser.parseAll(parser.exp, input).get
}
