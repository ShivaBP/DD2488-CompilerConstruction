package punkt0
package analyzer

import ast.Trees._

import Symbols._
import Types._

object TypeChecking extends Phase[Program, Program] {

  /** Typechecking does not produce a value, but has the side effect of
    * attaching types to trees and potentially outputting error messages. */
  def run(prog: Program)(ctx: Context): Program = {
    def tcExpr(expr: ExprTree, expected: Type*): Type = {
      val tpe: Type = expr match {
        case Null() =>
          expr.setType(TNull)
          TNull
        case IntLit(_) =>
          expr.setType(TInt)
          TInt
        case StringLit(_) =>
          expr.setType(TString)
          TString
        case True() =>
          expr.setType(TBoolean)
          TBoolean
        case False() =>
          expr.setType(TBoolean)
          TBoolean
        case This() =>
          expr.setType(expr.asInstanceOf[This].getSymbol.getType)
          expr.asInstanceOf[This].getSymbol.getType
        case Identifier(value) =>
          expr.setType(expr.asInstanceOf[Identifier].getType)
          expr.asInstanceOf[Identifier].getType
        case And(lhs, rhs) =>
          tcExpr(lhs, TBoolean)
          tcExpr(rhs, TBoolean)
          expr.setType(TBoolean)
          TBoolean
        case Or(lhs, rhs) =>
          tcExpr(lhs, TBoolean)
          tcExpr(rhs, TBoolean)
          expr.setType(TBoolean)
          TBoolean
        case Plus(lhs, rhs) =>
          tcExpr(lhs, TInt, TString)
          tcExpr(rhs, TInt, TString)
          (lhs.getType, rhs.getType) match {
            case (TString, TString) =>
              expr.setType(TString)
              TString
            case (TString, TInt) =>
              expr.setType(TString)
              TString
            case (TInt, TString) =>
              expr.setType(TString)
              TString
            case (TInt, TInt) =>
              expr.setType(TInt)
              TInt
            case (_, _) =>
              sys.error(
                "Invalid types " + lhs + " " + lhs.getType
                  .toString() + " and " + rhs + " " + rhs.getType
                  .toString() + " for the plus operator."
              )
          }
        case Minus(lhs, rhs) =>
          tcExpr(lhs, TInt)
          tcExpr(rhs, TInt)
          expr.setType(TInt)
          TInt
        case Times(lhs, rhs) =>
          tcExpr(lhs, TInt)
          tcExpr(rhs, TInt)
          expr.setType(TInt)
          TInt
        case Div(lhs, rhs) =>
          tcExpr(lhs, TInt)
          tcExpr(rhs, TInt)
          expr.setType(TInt)
          TInt
        case LessThan(lhs, rhs) =>
          tcExpr(lhs, TInt)
          tcExpr(rhs, TInt)
          expr.setType(TBoolean)
          TBoolean
        case Equals(lhs, rhs) =>
          tcExpr(lhs, TInt, TString, TBoolean, anyRef)
          tcExpr(rhs, TInt, TString, TBoolean, anyRef)
          if (lhs.getType == rhs.getType) {
            expr.setType(TBoolean)
            TBoolean
          } else if (lhs.getType.isSubTypeOf(anyRef) && rhs.getType.isSubTypeOf(
                       anyRef
                     )) {
            expr.setType(TBoolean)
            TBoolean
          } else {
            sys.error(
              "Invalid types: " + lhs.getType
                .toString() + " and " + rhs.getType
                .toString() + " for the equality comparison operator."
            )
          }
        case Not(in) =>
          tcExpr(in, TBoolean)
          expr.setType(TBoolean)
          TBoolean
        case New(tpe) =>
          val exprType = tcExpr(tpe, anyRef)
          expr.setType(exprType)
          exprType
        case MethodCall(obj, meth, args) =>
          val objType = tcExpr(obj)
          obj.setType(objType)
          var index: Int = 0
          for (arg <- args) {
            tcExpr(arg)
            var trueType =
              meth.getSymbol.asInstanceOf[MethodSymbol].argList(index).getType
            var currentType = arg.getType
            if (!currentType.isSubTypeOf(trueType)) {
              sys.error(
                "Invalid argument type:  \n Expected: " + trueType + "   \nReturned: " + currentType
                  .toString()
              )
            }
            index = index + 1
          }
          expr.setType(meth.getSymbol.getType)
          meth.getSymbol.asInstanceOf[MethodSymbol].getType
        case If(cond, thn, els) =>
          tcExpr(cond, TBoolean)
          els match {
            case Some(elsExpr) =>
              tcExpr(thn)
              tcExpr(elsExpr)
              if (thn.getType == elsExpr.getType) {
                expr.setType(thn.getType)
                thn.getType
              } else if ((thn.getType.equals(TNull) || thn.getType.equals(
                           TUnit
                         )) && elsExpr.getType
                           .isInstanceOf[TAnyRef]) {
                expr.setType(elsExpr.getType)
                elsExpr.getType
              } else if (thn.getType
                           .isInstanceOf[TAnyRef] && (elsExpr.getType.equals(
                           TNull
                         ) || elsExpr.getType.equals(TUnit))) {
                expr.setType(thn.getType)
                thn.getType
              } else if (thn.getType
                           .isInstanceOf[
                             TAnyRef
                           ] && elsExpr.getType
                           .isInstanceOf[
                             TAnyRef
                           ]) {
                var thnTypes = inheritType(thn.getType)
                var elsTypes = inheritType(elsExpr.getType)
                leastUpperBound(thnTypes, elsTypes) match {
                  case Some(typeFound) =>
                    expr.setType(typeFound)
                    typeFound
                  case None =>
                    sys.error(
                      "The types of if and else branch do not have a mutual inheritance and thus do not match."
                    )
                }
              } else {
                sys.error(
                  "The types of if and else branch statements must match."
                )
              }
            case None =>
              tcExpr(thn)
              thn.setType(thn.getType)
              expr.setType(TUnit)
              TUnit
          }
        case While(cond, body) =>
          tcExpr(cond, TBoolean)
          tcExpr(body)
          expr.setType(body.getType)
          body.getType
        case Println(in) =>
          tcExpr(in, TBoolean, TInt, TString)
          expr.setType(TUnit)
          TUnit
        case Assign(id, rest) =>
          tcExpr(id)
          tcExpr(rest)
          if (rest.getType.isSubTypeOf(id.getType)) {
            expr.setType(TUnit)
            TUnit
          } else {
            sys.error(
              "Invalid types: " + rest.getType
                .toString() + " has to be a subtype of " + id.getType
                .toString() + " for the assignment operator."
            )
          }
        case Block(exprs) =>
          for (expr <- exprs) {
            tcExpr(expr)
          }
          if (exprs.isEmpty) {
            expr.setType(TUnit)
            TUnit
          } else {
            expr.setType(exprs.last.getType)
            exprs.last.getType
          }
        case _ =>
          sys.error("The requested type is invalid.")
      }

      // Check result and return a valid type in case of error
      if (expected.isEmpty) {
        tpe
      } else if (!expected.exists(e => tpe.isSubTypeOf(e))) {
        Reporter.error(
          "Type error: expected: " + expected.toList
            .mkString(" or ") + ", found: " + tpe,
          expr
        )
        expected.head
      } else {
        tpe
      }
    }

    //Type checking for variables
    def tcVar(variable: VarDecl): Type = {
      var variableType = variable.tpe
      variableType match {
        case BooleanType() => tcExpr(variable.expr, TBoolean)
        case IntType()     => tcExpr(variable.expr, TInt)
        case StringType()  => tcExpr(variable.expr, TString)
        case UnitType()    => tcExpr(variable.expr, TUnit)
        case Identifier(value) =>
          tcExpr(variable.expr)
          if (variable.expr.getType.isSubTypeOf(variable.tpe.getType)) {
            TUnit
          } else {
            sys.error("The variable and its expression have unmatching types.")
          }
        case _ => sys.error("Invalid type for a varible declaration.")
      }
    }

    //Type conversion
    def tcTPE(tpe: TypeTree): Unit = {
      tpe match {
        case BooleanType() => tpe.setType(TBoolean)
        case IntType()     => tpe.setType(TInt)
        case StringType()  => tpe.setType(TString)
        case UnitType()    => tpe.setType(TUnit)
        case Identifier(value) =>
          for (classDecl <- prog.classes) {
            if (classDecl.id.value == value) {
              tpe.setType(TAnyRef(classDecl.getSymbol))
            }
          }
      }
    }

    //Handling parent inheritance
    def inheritType(tpe: Type): List[Type] = {
      val currentClassType = tpe.toString()
      var inheritedTypes = List[Type]()
      for (classDecl <- prog.classes) {
        if (classDecl.id.value == currentClassType) {
          var classSym = classDecl.getSymbol
          inheritedTypes = inheritedTypes ++ List(
            classSym.getType
          )
          classSym.parent match {
            case Some(parent) =>
              var currentParent = classSym.parent
              while (currentParent != None) {
                inheritedTypes = inheritedTypes ++ List(
                  currentParent.get.getType
                )
                currentParent = currentParent.get.parent
              }
            case None =>
          }
        }
      }
      inheritedTypes
    }

    //Parent least upperbound
    def leastUpperBound(
        thnTypes: List[Type],
        elsTypes: List[Type]
    ): Option[Type] = {
      for (thnType <- thnTypes) {
        for (elsType <- elsTypes) {
          if (thnType == elsType) {
            return Some(elsType)
          }
        }
      }
      None
    }

    //Type checking for each class
    for (classDecl <- prog.classes) {
      //Variable typecheck
      for (variable <- classDecl.vars) {
        tcVar(variable)
        tcExpr(variable.expr, variable.getSymbol.getType)
      }
      //Method typecheck
      for (meth <- classDecl.methods) {
        // Return type of the method
        tcTPE(meth.retType)
        //Variable typecheck
        for (variable <- meth.vars) {
          tcVar(variable)
          tcExpr(variable.expr, variable.getSymbol.getType)
        }
        //Expression typecheck
        for (expr <- meth.exprs) {
          tcExpr(expr)
        }
        //Return typecheck
        tcExpr(meth.retExpr)
        if (!meth.retExpr.getType.isSubTypeOf(meth.retType.getType)) {
          sys.error(
            "The type of method return expression " + meth.retExpr.getType
              .toString() + " must match the method return type " + meth.retType.getType
              .toString()
          )
        }
        //Overriding typecheck
        if (meth.overrides == true) {
          var methSym = meth.getSymbol
          var overridenMethSym = methSym.overridden.get
          //Arg typecheck
          var index = 0
          for (arg <- meth.args) {
            tcTPE(arg.tpe)
            var overridenMethArgType = overridenMethSym.argList(index).getType
            if (!arg.tpe.getType.isSubTypeOf(overridenMethArgType)) {
              sys.error(
                "The argument type of ovveride method does not match the argument type of its inherited method. "
              )
            }
            index = index + 1
          }
          //Return typecheck
          if (!overridenMethSym.getType.isSubTypeOf(methSym.getType)) {
            sys.error("Type matching for override modifier is violated.")
          }
        }
      }
    }

    //Type checking for main
    //Parent typecheck
    if (prog.main.parent.value != "App") {
      sys.error(
        "The main object declaration must extend the built-in App type."
      )
    }
    //Variable typecheck
    for (variable <- prog.main.vars) {
      tcVar(variable)
      tcExpr(variable.expr, variable.getSymbol.getType)
    }
    //Expression typecheck
    for (expr <- prog.main.exprs) {
      tcExpr(expr)
    }
    prog
  }
}
