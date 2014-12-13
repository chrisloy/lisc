package net.chrisloy.lisp

import scala.util.parsing.combinator._

class Parser {

  private def stripQuotes(x: String) = x.substring(1, x.length - 1)

  val parser = new JavaTokenParsers {

    def list: Parser[LList] =
      "(" ~> rep(exp) <~ ")" ^^ LList

    def string: Parser[LString] =
      stringLiteral ^^ stripQuotes ^^ LString

    def literal: Parser[LLiteral] =
      """[^() ]+""".r ^^ LLiteral

    def long: Parser[LLong] =
      wholeNumber ^^ (_.toLong) ^^ LLong

    def double: Parser[LDouble] =
      ("""\-?\d+\.\d*([eE]\-?\d+)?""".r | """\-?\d+[eE]\-?\d+""".r) ^^ (_.toDouble) ^^ LDouble

    def exp: Parser[Expression] =
      double | long | string | list | literal
  }

  def apply(input: String): Expression = parser.parseAll(parser.exp, input).get
}
