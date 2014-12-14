package net.chrisloy.lisp

import org.scalatest.prop.Checkers
import org.scalatest.{MustMatchers, FlatSpec}
import org.scalacheck.Arbitrary._
import org.scalacheck.Prop._

class ParserSpec extends FlatSpec with MustMatchers with Checkers {

  implicit val scope = new Scope

  val parse = new Parser

  it should "parse integers" in {
    check((x: Long) => parse(x.toString) == LLong(x))
  }

  it should "parse doubles" in {
    check((x: Double) => parse(x.toString) == LDouble(x))
  }

  it should "parse lists" in {
    parse("""("a" b (1 2))""") mustBe LList(List(LString("a"), LLiteral("b"), LList(List(LLong(1), LLong(2)))))
  }

  it should "add two integers" in {
    check((x: Long, y: Long) => parse(s"(+ $x $y)").value == x + y)
  }

  it should "add three integers" in {
    check((x: Long, y: Long, z: Long) => parse(s"(+ $x $y $z)").value == x + y + z)
  }

  it should "add with arbitrary nesting" in {
    check((x: Long, y: Long, z: Long) => parse(s"(+ $x (+ $y (+ $z)))").value == x + y + z)
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
    parse("[]") mustBe LVector(Nil)
  }

  it should "parse a vector of numbers" in {
    parse("[1 2 3]").value mustBe List(1, 2, 3)
  }
}
