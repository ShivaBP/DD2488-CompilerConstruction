package punkt0
package ast

import Trees._

object Printer {
  val tab: String = "  "

  def apply(t: Tree, doSymbolId: Boolean = false): String = {
    def strs(trees: List[Tree], sep: String): String =
      trees
        .map { t =>
          str(t)
        }
        .mkString(sep)

    def binaryExpr(lhs: Tree, rhs: Tree, sep: String): String =
      "(" + strs(List(lhs, rhs), sep) + ")"

    def tabb(s: String) =
      s.lines
        .map { x =>
          tab + x
        }
        .mkString("\n")

    def str(t: Tree): String = t match {
      case Program(main, classes) =>
        strs(classes, "\n\n") + "\n\n" + str(main)
      case MainDecl(objId, parent, vars, exprs) =>
        val head = "object " + str(objId) + " extends " + str(parent) + " {\n"
        val varDecls = if (vars.isEmpty) "" else strs(vars, "\n") + "\n"
        val expressions =
          if (exprs.size < 2) "" else strs(exprs.init, ";\n") + ";\n"
        val body = varDecls + expressions + (if (exprs.nonEmpty) str(exprs.last)
                                             else "")
        head + tabb(body) + "\n}"
      case ClassDecl(id, parent, vars, methods) =>
        val head = "class " + str(id) +
          (if (parent.isDefined)
             " extends " + str(parent.get)
           else "") + " {\n"
        val body = strs(vars, "\n") + (if (vars.isEmpty) "\n" else "\n\n") +
          strs(methods, "\n\n")
        head + tabb(body) + "\n\n}"
      case VarDecl(tpe, id, expr) =>
        "var " + str(id) + ": " + str(tpe) + " = " + str(expr) + ";"
      case MethodDecl(overrides, retType, id, args, vars, exprs, retExpr) =>
        val head = (if (overrides) "override " else "") + "def " + str(id) + "(" +
          strs(args, ", ") + "): " + str(retType) + " = {\n"
        val varDecls = if (vars.isEmpty) "" else strs(vars, "\n") + "\n"
        val expressions = if (exprs.isEmpty) "" else strs(exprs, ";\n") + ";\n"
        val body = varDecls + expressions + str(retExpr)
        head + tabb(body) + "\n}"
      case Formal(tpe, id) => str(id) + ": " + str(tpe)

      // types
      case IntType()     => "Int"
      case BooleanType() => "Boolean"
      case StringType()  => "String"
      case UnitType()    => "Unit"

      //Expressions
      case And(lhs, rhs)      => binaryExpr(lhs, rhs, " && ")
      case Or(lhs, rhs)       => binaryExpr(lhs, rhs, " || ")
      case Plus(lhs, rhs)     => binaryExpr(lhs, rhs, " + ")
      case Minus(lhs, rhs)    => binaryExpr(lhs, rhs, " - ")
      case Times(lhs, rhs)    => binaryExpr(lhs, rhs, " * ")
      case Div(lhs, rhs)      => binaryExpr(lhs, rhs, " / ")
      case LessThan(lhs, rhs) => binaryExpr(lhs, rhs, " < ")
      case Equals(lhs, rhs)   => binaryExpr(lhs, rhs, " == ")
      case MethodCall(obj, meth, args) =>
        str(obj) + "." + str(meth) + "(" + strs(args, ", ") + ")"
      case IntLit(value)    => value.toString
      case StringLit(value) => "\"" + value.toString + "\""
      case True()           => "true"
      case False()          => "false"
      case id @ Identifier(value) =>
        id._sym match {
          case Some(s) =>
            value.toString() + "#" + s.id
          case None =>
            value.toString() + "#??"
        }
      case This() =>
        t.asInstanceOf[This]._sym match {
          case Some(s) =>
            "this" + "#" + s.id
          case None =>
            "this" + "#??"
        }
      case Null()       => "null"
      case New(tpe)     => "new " + str(tpe) + "()"
      case Not(expr)    => "!(" + str(expr) + ")"
      case Block(exprs) => "{\n" + tabb(strs(exprs, ";\n")) + "\n}"
      case If(expr, thn, els) =>
        "if ( " + str(expr) + " ) " + str(thn) + (if (els.isDefined)
                                                    "\nelse " + str(els.get)
                                                  else "")
      case While(cond, body) => "while ( " + str(cond) + " ) " + str(body)
      case Println(expr)     => "println(" + str(expr) + ")"
      case Assign(id, expr)  => str(id) + " = " + str(expr)
      case _                 => ???
    }
    str(t)
  }
}
