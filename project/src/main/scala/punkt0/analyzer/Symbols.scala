package punkt0
package analyzer

import Types._

object Symbols {

  trait Symbolic[S <: Symbol] {
    var _sym: Option[S] = None

    def setSymbol(sym: S): this.type = {
      _sym = Some(sym)
      this
    }

    def getSymbol: S = _sym match {
      case Some(s) => s
      case None    => sys.error("Accessing undefined symbol.")
    }
  }

  sealed abstract class Symbol extends Positioned with Typed {
    val id: Int = ID.next
    val name: String
  }

  private object ID {
    private var c: Int = 1

    def next: Int = {
      val ret = c
      c = c + 1
      ret
    }
  }

  class GlobalScope {
    var mainClass: ClassSymbol = _
    var classes = Map[String, ClassSymbol]()

    def lookupClass(n: String): Option[ClassSymbol] = {
      var toReturn: Option[ClassSymbol] = None
      if (classes.contains(n)) {
        val toGet = classes.get(n)
        toReturn = Some(toGet.get)
      }
      toReturn
    }
  }

  class ClassSymbol(val name: String) extends Symbol {
    var parent: Option[ClassSymbol] = None
    var methods = Map[String, MethodSymbol]()
    var members = Map[String, VariableSymbol]()

    def lookupMethod(n: String): Option[MethodSymbol] = {
      var toReturn: Option[MethodSymbol] = None
      if (methods.contains(n)) {
        val toGet = methods.get(n)
        toReturn = Some(toGet.get)
      } else {
        var currentParent = parent
        while (currentParent != None && toReturn == None) {
          toReturn = currentParent.get.lookupMethod(n)
          currentParent = currentParent.get.parent
        }
      }
      toReturn
    }

    def lookupVar(n: String): Option[VariableSymbol] = {
      var toReturn: Option[VariableSymbol] = None
      if (members.contains(n)) {
        val toGet = members.get(n)
        toReturn = Some(toGet.get)
      } else {
        var currentParent = parent
        while (currentParent != None && toReturn == None) {
          toReturn = currentParent.get.lookupVar(n)
          currentParent = currentParent.get.parent
        }
      }
      toReturn
    }
  }

  class MethodSymbol(val name: String, val classSymbol: ClassSymbol)
      extends Symbol {
    var params = Map[String, VariableSymbol]()
    var members = Map[String, VariableSymbol]()
    var argList: List[VariableSymbol] = Nil
    var overridden: Option[MethodSymbol] = None

    def lookupVar(n: String): Option[VariableSymbol] = {
      var toReturn: Option[VariableSymbol] = None
      if (params.contains(n)) {
        val toGet = params.get(n)
        toReturn = Some(toGet.get)
      } else if (members.contains(n)) {
        val toGet = members.get(n)
        toReturn = Some(toGet.get)
      } else {
        toReturn = classSymbol.lookupVar(n)
      }
      toReturn
    }
  }

  class VariableSymbol(val name: String) extends Symbol
}
