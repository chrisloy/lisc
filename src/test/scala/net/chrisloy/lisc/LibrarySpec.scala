package net.chrisloy.lisc

import org.scalatest.prop.Checkers
import org.scalatest.{MustMatchers, FlatSpec}

class LibrarySpec extends FlatSpec with MustMatchers with Checkers {

  implicit val scope = new Scope

  val parse = new Parser

  "equality" should "handle numbers" ignore {
    check((x: Long) => parse(s"(= $x $x)").value == true)
    check((x: Long, y: Long) => parse(s"(= $x $y)").value == (x == y))
  }
}
