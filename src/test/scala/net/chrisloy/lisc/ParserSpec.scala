package net.chrisloy.lisc

import org.scalatest.prop.Checkers
import org.scalatest.{MustMatchers, FlatSpec}
import org.scalacheck.Arbitrary._
import org.scalacheck.Prop._

class ParserSpec extends FlatSpec with MustMatchers with Checkers {

  implicit val scope = Scope()
  import scope.eval

  val parse = new Parser

  it should "parse integers" in {
    check((x: Long) => eval(parse(x.toString)) == x)
  }

  it should "parse doubles" in {
    check((x: Double) => eval(parse(x.toString)) == x)
  }

  it should "handle booleans" in {
    eval(parse("true")) mustBe true
    eval(parse("false")) mustBe false
  }

  it should "handle conditionals" in {
    eval(parse("(if true 1 2)")) mustBe 1
    eval(parse("(if false 1 2)")) mustBe 2
  }

  it should "parse an empty vector" in {
    eval(parse("[]")) mustBe Nil
  }

  it should "parse a vector of numbers" in {
    eval(parse("[1 2 3]")) mustBe List(1, 2, 3)
  }

  it should "handle functions returning symbols" in {
    eval(parse("((if true + fn) 1 2)")) mustBe 3
  }

  it should "create a zero-arity function" in {
    eval(parse("(fn [] (+ 1 2))")).asInstanceOf[Eval](scope)(Nil) mustBe 3
  }

  it should "execute a zero-arity function" in {
    eval(parse("((fn [] (+ 1 2)))")) mustBe 3
  }

  it should "register a zero-arity function" in {
    eval(parse("(defn a [] (+ 1 2))"))
    eval(parse("(a)")) mustBe 3
  }

  it should "create a one-arity function" in {
    eval(parse("(fn [a] (+ 1 a))")).asInstanceOf[Eval](scope)(LLong(3) :: Nil) mustBe 4
  }

  it should "allow functions bound as values" in {
    eval(parse("(def x (fn [] (+ 1 2)))"))
    eval(parse("(x)")) mustBe 3
    eval(parse("(def add2 (fn [x] (+ x 2)))"))
    eval(parse("(add2 4)")) mustBe 6
  }

  it should "return the value of the first expression in a program" in {
    eval(parse("1 2 3 4")) mustBe 1
  }
}
