package net.chrisloy.lisp

import scala.collection.mutable

class Scope {

  private val atoms = mutable.Map.empty[String, Expression]

  def apply(atom: String): Expression = atoms(atom)
}
