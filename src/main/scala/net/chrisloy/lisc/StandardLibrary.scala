package net.chrisloy.lisc

object StandardLibrary {

  def loadInto(scope: Scope)(parse: Parser): Unit = scope.eval {
    parse {
      io.Source.fromInputStream(getClass.getResourceAsStream("/std-lib.lisp")).mkString
    }
  }
}
