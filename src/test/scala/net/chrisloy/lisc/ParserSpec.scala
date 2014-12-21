package net.chrisloy.lisc

import org.scalatest.prop.Checkers
import org.scalatest.{MustMatchers, FlatSpec}
import org.scalacheck.Arbitrary._
import org.scalacheck.Prop._

class ParserSpec extends FlatSpec with MustMatchers with Checkers {

  implicit val scope = Scope()

  val parse = new Parser

  it should "parse integers" in {
    check((x: Long) => parse(x.toString).value == x)
  }

  it should "parse doubles" in {
    check((x: Double) => parse(x.toString).value == x)
  }

  it should "handle booleans" in {
    parse("true").value mustBe true
    parse("false").value mustBe false
  }

  it should "handle conditionals" in {
    parse("(if true 1 2)").value mustBe 1
    parse("(if false 1 2)").value mustBe 2
  }

  it should "parse an empty vector" in {
    parse("[]").value mustBe Nil
  }

  it should "parse a vector of numbers" in {
    parse("[1 2 3]").value mustBe List(1, 2, 3)
  }

  it should "handle functions returning symbols" in {
    parse("((if true + fn) 1 2)").value mustBe 3
  }

  it should "create a zero-arity function" in {
    parse("(fn [] (+ 1 2))").value.asInstanceOf[Eval](scope)(Nil) mustBe 3
  }

  it should "execute a zero-arity function" in {
    parse("((fn [] (+ 1 2)))").value mustBe 3
  }

  it should "register a zero-arity function" in {
    parse("(defn a [] (+ 1 2))").value
    parse("(a)").value mustBe 3
  }

  it should "create a one-arity function" in {
    parse("(fn [a] (+ 1 a))").value.asInstanceOf[Eval](scope)(LLong(3) :: Nil) mustBe 4
  }
}
