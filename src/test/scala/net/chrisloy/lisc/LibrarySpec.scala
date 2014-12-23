package net.chrisloy.lisc

import org.scalatest.prop.Checkers
import org.scalatest.{MustMatchers, FlatSpec}

import scala.util.control.NonFatal

class LibrarySpec extends FlatSpec with MustMatchers with Checkers {

  implicit val scope = Scope()
  import scope.eval

  val parse = new Parser

  "(+ ...)" should "add two integers" in {
    check((x: Long, y: Long) => eval(parse(s"(+ $x $y)")) == x + y)
  }

  it should "add three integers" in {
    check((x: Long, y: Long, z: Long) => eval(parse(s"(+ $x $y $z)")) == x + y + z)
  }

  it should "add with arbitrary nesting" in {
    check((x: Long, y: Long, z: Long) => eval(parse(s"(+ $x (+ $y (+ $z)))")) == x + y + z)
  }

  "(= ...)" should "handle numbers" in {
    check((x: Long) => eval(parse(s"(= $x $x)")) == true)
    check((x: Long, y: Long) => eval(parse(s"(= $x $y)")) == (x == y))
  }

  it should "handle strings" in {
    eval(parse(s"""(= "" "")""")) mustBe true
    eval(parse(s"""(= "pies" "pies")""")) mustBe true
    eval(parse(s"""(= "pies" "cakes")""")) mustBe false
  }

  "(< ...)" should "compare correctly" in {
    check((x: Long, y: Long) => eval(parse(s"(< $x $y)")) == (x < y))
    check((x: Long, y: Double) => eval(parse(s"(< $x $y)")) == (x < y))
    check((x: Double, y: Long) => eval(parse(s"(< $x $y)")) == (x < y))
    check((x: Double, y: Double) => eval(parse(s"(< $x $y)")) == (x < y))
  }

  "(- ...)" should "subtract correctly" in {
    check((x: Long, y: Long) => eval(parse(s"(- $x $y)")) == (x - y))
    check((x: Long, y: Double) => eval(parse(s"(- $x $y)")) == (x - y))
    check((x: Double, y: Long) => eval(parse(s"(- $x $y)")) == (x - y))
    check((x: Double, y: Double) => eval(parse(s"(- $x $y)")) == (x - y))
  }

  "(! ...)" should "invert booleans" in {
    eval(parse("(! true)")) mustBe false
    eval(parse("(! false)")) mustBe true
  }

  "(<= ...)" should "work properly" in {
    eval(parse("(<= 3 4)")) mustBe true
    eval(parse("(<= 3 3)")) mustBe true
    eval(parse("(<= 3 3.0)")) mustBe true
    eval(parse("(<= 4 2)")) mustBe false
  }

  "(&& ...)" should "return true only if all arguments are booleans" in {
    eval(parse("(&& true)")) mustBe true
    eval(parse("(&& false)")) mustBe false
    eval(parse("(&& true true true true)")) mustBe true
    eval(parse("(&& true true false true)")) mustBe false
  }

  "(* ...)" should "work" in {
    eval(parse("(* 1 2 3 4 5)")) mustBe 120
    check((x: Long, y: Long) => eval(parse(s"(* $x $y)")) == (x * y))
    check((x: Long, y: Double) => eval(parse(s"(* $x $y)")) == (x * y))
    check((x: Double, y: Long) => eval(parse(s"(* $x $y)")) == (x * y))
    check((x: Double, y: Double) => eval(parse(s"(* $x $y)")) == (x * y))
  }

  "(/ ...)" should "work" in {
    eval(parse("(/ 1000 10 5)")) mustBe 20
    def mustBeZero(y: Any): PartialFunction[Throwable, Boolean] = { case NonFatal(ex) => y == 0 }
    check((x: Long, y: Long) => try eval(parse(s"(/ $x $y)")) == (x / y) catch mustBeZero(y))
    check((x: Long, y: Double) => try eval(parse(s"(/ $x $y)")) == (x / y) catch mustBeZero(y))
    check((x: Double, y: Long) => try eval(parse(s"(/ $x $y)")) == (x / y) catch mustBeZero(y))
    check((x: Double, y: Double) => try eval(parse(s"(/ $x $y)")) == (x / y) catch mustBeZero(y))
  }
}
