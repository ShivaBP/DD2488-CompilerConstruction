package punkt0
package analyzer

import Symbols._

object Types {

  trait Typed {
    private var _tpe: Type = TUntyped

    def setType(tpe: Type): this.type = { _tpe = tpe; this }
    def getType: Type = _tpe
    def getTypeStr: String = {
      try getType.toString
      catch {
        case t: Throwable => "??"
      }
    }
  }

  sealed abstract class Type {
    def isSubTypeOf(tpe: Type): Boolean
  }

  case object TError extends Type {
    override def isSubTypeOf(tpe: Type): Boolean = true
    override def toString = "[error]"
  }
  case object TUntyped extends Type {
    override def isSubTypeOf(tpe: Type): Boolean = false
    override def toString = "[untyped]"
  }
  case object TNull extends Type {
    override def isSubTypeOf(tpe: Type): Boolean = tpe match {
      case TAnyRef(_) => true
      case AnyRef     => true
      case _          => false
    }
    override def toString = "Null"
  }
  case object TInt extends Type {
    override def isSubTypeOf(tpe: Type): Boolean = tpe match {
      case TInt => true
      case _    => false
    }
    override def toString = "Int"
  }
  case object TBoolean extends Type {
    override def isSubTypeOf(tpe: Type): Boolean = tpe match {
      case TBoolean => true
      case _        => false
    }
    override def toString = "Boolean"
  }
  case object TString extends Type {
    override def isSubTypeOf(tpe: Type): Boolean = tpe match {
      case TString => true
      case _       => false
    }
    override def toString = "String"
  }
  case object TUnit extends Type {
    override def isSubTypeOf(tpe: Type): Boolean = tpe match {
      case TUnit => true
      case _ =>
        false
    }
    override def toString = "Unit"
  }

  case class TAnyRef(classSymbol: ClassSymbol) extends Type {
    override def isSubTypeOf(tpe: Type): Boolean = {
      var toReturn = false
      if (tpe == classSymbol.getType) {
        toReturn = true
      } else if (tpe == anyRef) {
        toReturn = true
      } else {
        classSymbol.parent match {
          case Some(parentFound) =>
            toReturn = parentFound.getType.isSubTypeOf(tpe)

          case None => toReturn = false
        }
      }
      toReturn
    }
    override def toString = classSymbol.name
  }

  // special object to implement the fact that all objects are its subclasses
  val anyRef = TAnyRef(new ClassSymbol("AnyRef"))
}
