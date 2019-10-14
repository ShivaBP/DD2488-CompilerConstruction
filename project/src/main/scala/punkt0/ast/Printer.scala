package punkt0
package ast

import Trees._

object Printer {
  def apply(t: Tree): String = {
    val sb: StringBuilder = new StringBuilder

    def printIdentifier(ident: Identifier): String = {
      sb.append(ident.asInstanceOf[Identifier].value)
      sb.toString()
    }

    def printType(typeValue: TypeTree): Unit = {
      typeValue match {
        case BooleanType() => sb.append("Bool")
        case IntType()     => sb.append("Int")
        case StringType()  => sb.append("String")
        case Identifier(_) =>
          sb.append(typeValue.asInstanceOf[Identifier].value)
        case _ => sb.append(" No such type ")
      }
    }

    def printExpression(expression: ExprTree): String = {
      expression match {
        case Or(_, _) =>
          val or = expression.asInstanceOf[Or]
          sb.append("(")
          sb.append(printExpression(or.lhs))
          sb.append(" || ")
          sb.append(printExpression(or.rhs))
          sb.append(")")
        case And(_, _) =>
          val and = expression.asInstanceOf[And]
          sb.append("(")
          sb.append(printExpression(and.lhs))
          sb.append(" && ")
          sb.append(printExpression(and.rhs))
          sb.append(")")
        case Equals(_, _) =>
          val eq = expression.asInstanceOf[Equals]
          sb.append("(")
          sb.append(printExpression(eq.lhs))
          sb.append(" == ")
          sb.append(printExpression(eq.rhs))
          sb.append(")")
        case LessThan(_, _) =>
          val less = expression.asInstanceOf[LessThan]
          sb.append("(")
          sb.append(printExpression(less.lhs))
          sb.append(" < ")
          sb.append(printExpression(less.rhs))
          sb.append(")")
        case Plus(_, _) =>
          val plus = expression.asInstanceOf[Plus]
          sb.append("(")
          sb.append(printExpression(plus.lhs))
          sb.append(" + ")
          sb.append(printExpression(plus.rhs))
          sb.append(")")
        case Minus(_, _) =>
          val minus = expression.asInstanceOf[Minus]
          sb.append("(")
          sb.append(printExpression(minus.lhs))
          sb.append(" - ")
          sb.append(printExpression(minus.rhs))
          sb.append(")")
        case Times(_, _) =>
          val mul = expression.asInstanceOf[Times]
          sb.append("(")
          sb.append(printExpression(mul.lhs))
          sb.append(" * ")
          sb.append(printExpression(mul.rhs))
          sb.append(")")
        case Div(_, _) =>
          val divide = expression.asInstanceOf[Div]
          sb.append("(")
          sb.append(printExpression(divide.lhs))
          sb.append(" / ")
          sb.append(printExpression(divide.rhs))
          sb.append(")")
        case True()    => sb.append("true")
        case False()   => sb.append("false")
        case This()    => sb.append("this")
        case Null()    => sb.append("null")
        case IntLit(_) => sb.append(expression.asInstanceOf[IntLit].value)
        case StringLit(_) =>
          sb.append("\" " + expression.asInstanceOf[StringLit].value + " \"")
        case Identifier(_) =>
          sb.append(expression.asInstanceOf[Identifier].value)
        case Assign(_, _) =>
          val assignment = expression.asInstanceOf[Assign]
          printIdentifier(assignment.id)
          sb.append(" = ")
          printExpression(assignment.expr)
          sb.append(";\n")
        case New(_) =>
          val newObj = expression.asInstanceOf[New]
          sb.append("new " + printType(newObj.tpe) + "()")
        case Not(_) =>
          val variable = expression.asInstanceOf[Not]
          sb.append("!")
          printExpression(variable.expr)
        case MethodCall(_, _, _) =>
          val methodCaller = expression.asInstanceOf[MethodCall]
          printExpression(methodCaller.obj)
          sb.append("." + methodCaller.meth.value + "( ")
          if (methodCaller.args.length > 0) {
            printExpression(methodCaller.args.head)
            if (methodCaller.args.length > 1) {
              for (arg <- methodCaller.args.tail) {
                sb.append(", ")
                printExpression(arg)
              }
            }
          }
        case Block(_) =>
          val blockStats = expression.asInstanceOf[Block]
          sb.append("{ ")
          if (blockStats.exprs.length > 0) {
            printExpression(blockStats.exprs.head)
            if (blockStats.exprs.length > 1) {
              for (expr <- blockStats.exprs.tail) {
                sb.append("; ")
                printExpression(expr)
              }
            }
          }
        case If(_, _, _) =>
          val ifStat = expression.asInstanceOf[If]
          sb.append("if ( ")
          printExpression(ifStat.expr)
          sb.append(" ) ")
          printExpression(ifStat.thn)
          if (!(ifStat.els == None)) {
            sb.append("    else ")
            printExpression(ifStat.els.get)
          }
        case While(_, _) =>
          val whileStat = expression.asInstanceOf[While]
          sb.append("while (")
          printExpression(whileStat.cond)
          sb.append(" )")
          printExpression(whileStat.body)
        case Println(_) =>
          val printStat = expression.asInstanceOf[Println]
          sb.append("println")
          printExpression(printStat.expr)
      }
      sb.toString()
    }

    def printVar(varDecl: VarDecl): Unit = {
      sb.append("var")
      printIdentifier(varDecl.id)
      sb.append(" : ")
      printType(varDecl.tpe)
      sb.append(" = ")
      printExpression(varDecl.expr)
      sb.append(";\n")
    }

    def printFormal(formal: Formal): Unit = {
      printIdentifier(formal.id)
      sb.append(" : ")
      printType(formal.tpe)
    }

    def printMethod(methodDecl: MethodDecl): Unit = {
      if (methodDecl.overrides == true) {
        sb.append("override ")
      }
      sb.append("def")
      printIdentifier(methodDecl.id)
      sb.append("( ")
      if (methodDecl.args.length > 0) {
        printFormal(methodDecl.args.head)
        if (methodDecl.args.length > 1) {
          for (arg <- methodDecl.args.tail) {
            sb.append(", ")
            printFormal(arg)
          }
        }
      }
      sb.append(" ) :")
      printType(methodDecl.retType)
      sb.append(" ={ ")
      if (methodDecl.vars.length != 0) {
        for (varDecl <- methodDecl.vars) {
          printVar(varDecl)
        }
      }
      for (expr <- methodDecl.exprs) {
        printExpression(expr)
      }
      sb.append(" }\n")
    }

    def printMain(mainDecl: MainDecl): Unit = {
      sb.append("object ")
      printIdentifier(mainDecl.obj)
      sb.append(" extends ")
      printIdentifier(mainDecl.parent)
      sb.append(" { ")
      if (mainDecl.vars.length != 0) {
        for (varDecl <- mainDecl.vars) {
          printVar(varDecl)
        }
      }
      for (expr <- mainDecl.exprs) {
        printExpression(expr)
      }
      sb.append(" }\n ")
    }

    def printClass(classDecl: ClassDecl): Unit = {
      sb.append("class ")
      printIdentifier(classDecl.id)
      if (!(classDecl.parent == None)) {
        sb.append(" extends ")
        sb.append(classDecl.parent.get.value)
      }
      sb.append(" { ")
      if (classDecl.vars.length != 0) {
        for (varDecl <- classDecl.vars) {
          printVar(varDecl)
        }
      }
      if (classDecl.methods.length != 0) {
        for (method <- classDecl.methods) {
          printMethod(method)
        }
      }
      sb.append(" }\n ")
    }

    val prog = t.asInstanceOf[Program]
    if (prog.classes.length != 0) {
      for (theClass <- prog.classes) {
        printClass(theClass)
      }
    }
    printMain(prog.main)
    sb.toString()
  }
}
