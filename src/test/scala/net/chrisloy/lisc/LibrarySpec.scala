package net.chrisloy.lisc

import org.scalatest.prop.Checkers
import org.scalatest.{MustMatchers, FlatSpec}

class LibrarySpec extends FlatSpec with MustMatchers with Checkers {

  implicit val scope = Scope()

  val parse = new Parser

  "(+ ...)" should "add two integers" in {
    check((x: Long, y: Long) => parse(s"(+ $x $y)").value == x + y)
  }

  it should "add three integers" in {
    check((x: Long, y: Long, z: Long) => parse(s"(+ $x $y $z)").value == x + y + z)
  }

  it should "add with arbitrary nesting" in {
    check((x: Long, y: Long, z: Long) => parse(s"(+ $x (+ $y (+ $z)))").value == x + y + z)
  }

  "(= ...)" should "handle numbers" in {
    check((x: Long) => parse(s"(= $x $x)").value == true)
    check((x: Long, y: Long) => parse(s"(= $x $y)").value == (x == y))
  }

  it should "handle strings" in {
    parse(s"""(= "" "")""").value mustBe true
    parse(s"""(= "pies" "pies")""").value mustBe true
    parse(s"""(= "pies" "cakes")""").value mustBe false
  }

  "(< ...)" should "compare correctly" in {
    check((x: Long, y: Long) => parse(s"(< $x $y)").value == (x < y))
    check((x: Long, y: Double) => parse(s"(< $x $y)").value == (x < y))
    check((x: Double, y: Long) => parse(s"(< $x $y)").value == (x < y))
    check((x: Double, y: Double) => parse(s"(< $x $y)").value == (x < y))
  }

  "(- ...)" should "subtract correctly" in {
    check((x: Long, y: Long) => parse(s"(- $x $y)").value == (x - y))
    check((x: Long, y: Double) => parse(s"(- $x $y)").value == (x - y))
    check((x: Double, y: Long) => parse(s"(- $x $y)").value == (x - y))
    check((x: Double, y: Double) => parse(s"(- $x $y)").value == (x - y))
  }

  "(! ...)" should "invert booleans" in {
    parse("(! true)").value mustBe false
    parse("(! false)").value mustBe true
  }

  "(&& ...)" should "return true only if all arguments are booleans" in {
    parse("(&& true)").value mustBe true
    parse("(&& false)").value mustBe false
    parse("(&& true true true true)").value mustBe true
    parse("(&& true true false true)").value mustBe false
  }
}
