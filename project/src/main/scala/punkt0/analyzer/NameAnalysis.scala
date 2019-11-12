package punkt0
package analyzer

import ast.Trees._
import Symbols._
import punkt0.ast.Trees
import ast.Trees.ExprTree
import punkt0.analyzer.Types.TAnyRef
import punkt0.analyzer.Types.TInt
import punkt0.analyzer.Types.TString
import punkt0.analyzer.Types.TBoolean
import punkt0.analyzer.Types.TUnit
import punkt0.analyzer.Types.TUntyped
import punkt0.analyzer.Types.anyRef
import punkt0.analyzer.Types.TNull
import punkt0.analyzer.Types.Type

object NameAnalysis extends Phase[Program, Program] {

  def run(prog: Program)(ctx: Context): Program = {
    import Reporter._

    //Global Symbol
    val globalSymbol = new GlobalScope

    //Type conversion
    def tpeToType(tpe: TypeTree): Type = {
      tpe match {
        case IntType()     => TInt
        case StringType()  => TString
        case BooleanType() => TBoolean
        case UnitType()    => TUnit
        case Identifier(value) =>
          globalSymbol.lookupClass(value) match {
            case Some(classFound) =>
              tpe.asInstanceOf[Identifier].setSymbol(classFound)
              classFound.getType
            case None =>
              sys.error("The requested type " + value + " does not exist.")
          }
        case _ => sys.error("The requested type does not exist.")
      }
    }

    //Class declarations
    for (classDecl <- prog.classes) {
      val className: String = classDecl.id.value
      val classSym = new ClassSymbol(className)
      classDecl.setSymbol(classSym)
      classDecl.id.setSymbol(classSym)
      classSym.setType(new TAnyRef(classSym))
      classSym.setPos(classDecl)
      if (globalSymbol.lookupClass(className) != None) {
        sys.error(
          "Another class with the name " + className + " already exists."
        )
      } else {
        globalSymbol.classes = globalSymbol.classes + (classDecl.id.value -> classSym)
      }
    }

    //Class parent
    for (classDecl <- prog.classes) {
      val classSym = classDecl.getSymbol
      if (classDecl.parent != None) {
        var parent = classDecl.parent
        globalSymbol.lookupClass(parent.get.value) match {
          case Some(parentFound) =>
            classDecl.parent.get.setSymbol(parentFound)
            classSym.parent = Some(parentFound)
          case None =>
            sys.error(
              "The superclass " + parent.get.value + " has not been declared"
            )
        }
        var currentParent = classSym.parent
        while (currentParent != None) {
          if (currentParent.get.name == classSym.name) {
            sys.error(
              "The transitive closure of the inheritance relation has been violated."
            )
          } else {
            currentParent = currentParent.get.parent
          }
        }
      }
    }

    //Elements within classes
    for (classDecl <- prog.classes) {
      val classSym = classDecl.getSymbol
      //Class members
      for (classVar <- classDecl.vars) {
        val varName: String = classVar.id.value
        val varSymbol = new VariableSymbol(varName)
        classVar.setSymbol(varSymbol)
        classVar.id.setSymbol(varSymbol)
        varSymbol.setType(tpeToType(classVar.tpe))
        varSymbol.setPos(classVar)
        if (classSym.lookupVar(varName) != None) {
          sys.error(
            "Another variable with the name " + varName + " already exists in the current scope."
          )
        } else {
          classVar.expr match {
            case Null() | IntLit(_) | StringLit(_) | True() | False() |
                New(_) =>
              methodExprs(classVar.expr, classSym)
              classVar.expr.setType(classVar.expr.getType)
              classSym.members = classSym.members + (varSymbol.name -> varSymbol)
            case _ =>
              sys.error(
                "Invalid initialization for variable " + varName + "."
              )
          }
        }
      }

      //Class method declarations
      var declaredMeths = List[String]()
      for (meth <- classDecl.methods) {
        val methodName: String = meth.id.value
        val methClass = classDecl.getSymbol
        val methodSym = new MethodSymbol(methodName, methClass)
        meth.setSymbol(methodSym)
        meth.id.setSymbol(methodSym)
        methodSym.setPos(meth)
        methodSym.setType(tpeToType(meth.retType))
        if (declaredMeths.contains(methodName)) {
          sys.error(
            "Another method with the name " + methodName + " already exists in the current scope."
          )
        } else {
          if (meth.overrides == false) {
            if (methClass.parent != None) {
              methClass.parent.get.lookupMethod(methodName) match {
                case Some(duplicateFound) =>
                  sys.error(
                    "Another method with the name " + methodName + " already exists in the current scope."
                  )
                case None =>
                  declaredMeths = declaredMeths ++ List(methodName)
                  classSym.methods = classSym.methods + (meth.id.value -> methodSym)
              }
            } else {
              declaredMeths = declaredMeths ++ List(methodName)
              classSym.methods = classSym.methods + (meth.id.value -> methodSym)
            }
          } else {
            declaredMeths = declaredMeths ++ List(methodName)
            classSym.methods = classSym.methods + (meth.id.value -> methodSym)
          }
        }

        //Method Arglist and method params
        var declaredMembers = List[String]()
        var argList = List[VariableSymbol]()
        for (arg <- meth.args) {
          val argName: String = arg.id.value
          val argSymbol = new VariableSymbol(argName)
          arg.setSymbol(argSymbol)
          arg.id.setSymbol(argSymbol)
          argSymbol.setPos(arg)
          argSymbol.setType(tpeToType(arg.tpe))
          if (declaredMembers.contains(argName)) {
            sys.error(
              "Another parameter with the name " + argName + " has already been declared."
            )
          } else {
            declaredMembers = declaredMembers ++ List(argName)
            argList = argList ++ List(argSymbol)
            methodSym.params = methodSym.params + (argName -> argSymbol)
          }
        }
        methodSym.argList = argList

        //Method members
        for (methVar <- meth.vars) {
          val varName: String = methVar.id.value
          val varSymbol = new VariableSymbol(varName)
          methVar.setSymbol(varSymbol)
          methVar.id.setSymbol(varSymbol)
          varSymbol.setType(tpeToType(methVar.tpe))
          if (meth.overrides == false) {
            if (declaredMembers.contains(varName)) {
              sys.error(
                "Another variable with the name " + varName + " already exists in the method " + methodName + "."
              )
            } else {
              methVar.expr match {
                case Null() | IntLit(_) | StringLit(_) | True() | False() |
                    New(_) =>
                  methodExprs(methVar.expr, methodSym)
                  methVar.expr.setType(methVar.expr.getType)
                  methodSym.members = methodSym.members + (varSymbol.name -> varSymbol)
                case _ =>
                  sys.error(
                    "Invalid initialization for variable " + varName + "."
                  )
              }
            }
          } else {
            var currentParent = methClass.parent
            var varExists = false
            while (currentParent != None && varExists == false) {
              currentParent.get.lookupVar(varName) match {
                case Some(varFound) =>
                  sys.error(
                    "Variable " + varName + " is already declared in a superclass and cannot be overridden."
                  )
                  varExists = true
                case None =>
                  currentParent = currentParent.get.parent
              }
            }
            if (varExists == false) {
              methVar.expr match {
                case Null() | IntLit(_) | StringLit(_) | True() | False() |
                    New(_) =>
                  methodExprs(methVar.expr, classSym)
                  methVar.expr.setType(methVar.expr.getType)
                  methodSym.members = methodSym.members + (varSymbol.name -> varSymbol)
                case _ =>
                  sys.error(
                    "Invalid initialization for variable " + varName + "."
                  )
              }
            }
          }
        }
      }
    }

    //Handle overriding
    for (classDecl <- prog.classes) {
      for (meth <- classDecl.methods) {
        val methodName: String = meth.id.value
        val methClass = classDecl.getSymbol
        val methodSym = meth.getSymbol
        val overrideType = tpeToType(meth.retType)
        //Method override
        if (meth.overrides == true) {
          if (methClass.parent == None) {
            sys.error("Invalid use of override keyword.")
          } else {
            val supercalss = methClass.parent.get
            supercalss.lookupMethod(methodName) match {
              case Some(duplicateFound) =>
                val overrideParamLength = methodSym.params.size
                val overriddenParamLength = duplicateFound.params.size
                if (overriddenParamLength == overrideParamLength) {
                  methodSym.overridden = Some(duplicateFound)
                } else {
                  sys.error(
                    "The number of parameters of the override method " + methodName + " is invalid."
                  )
                }
              case None =>
                sys.error(
                  "Override method " + methodName + " does not exist in a superclass."
                )
            }
          }
        }
      }
      for (meth <- classDecl.methods) {
        for (expr <- meth.exprs) {
          methodExprs(expr, meth.getSymbol)
        }
        methodExprs(meth.retExpr, meth.getSymbol)
      }
    }

    //Handle expressions
    def methodExprs(expr: ExprTree, location: Symbol): Unit = {
      expr match {
        case IntLit(_)    =>
        case StringLit(_) =>
        case True()       =>
        case False()      =>
        case Null()       =>
        case This() =>
          val classSym = location.asInstanceOf[MethodSymbol].classSymbol
          expr.asInstanceOf[This].setSymbol(classSym)
        case And(lhs, rhs) =>
          methodExprs(lhs, location)
          methodExprs(rhs, location)
        case Or(lhs, rhs) =>
          methodExprs(lhs, location)
          methodExprs(rhs, location)
        case Plus(lhs, rhs) =>
          methodExprs(lhs, location)
          methodExprs(rhs, location)
        case Minus(lhs, rhs) =>
          methodExprs(lhs, location)
          methodExprs(rhs, location)
        case Times(lhs, rhs) =>
          methodExprs(lhs, location)
          methodExprs(rhs, location)
        case Div(lhs, rhs) =>
          methodExprs(lhs, location)
          methodExprs(rhs, location)
        case LessThan(lhs, rhs) =>
          methodExprs(lhs, location)
          methodExprs(rhs, location)
        case Equals(lhs, rhs) =>
          methodExprs(lhs, location)
          methodExprs(rhs, location)
        case Not(in) =>
          methodExprs(in, location)
        case New(tpe) =>
          val link = globalSymbol.lookupClass(tpe.value)
          link match {
            case Some(classFound) =>
              tpe.setSymbol(classFound)
            case None =>
              sys.error("The class " + tpe.value + " has not beeen declared.")
          }
        case MethodCall(obj, meth, args) =>
          methodExprs(obj, location)
          obj match {
            case This() =>
              val objSym = location.asInstanceOf[MethodSymbol].classSymbol
              val thisMethod = objSym.lookupMethod(meth.value)
              thisMethod match {
                case Some(methodFound) =>
                  val calledArgsLength: Int = args.length
                  val originalArgsLength: Int = methodFound.argList.length
                  if (calledArgsLength == originalArgsLength) {
                    obj.asInstanceOf[This].setSymbol(objSym)
                    meth.setSymbol(methodFound)
                  } else {
                    sys.error(
                      "The number of arguments given does not match the number of arguments required for method " + meth.value + "."
                    )
                  }
                case None =>
                  sys.error(
                    "The method " + meth.value + " has not been declared inside the class ."
                  )
              }
            case Identifier(variable) =>
              location.asInstanceOf[MethodSymbol].lookupVar(variable) match {
                case Some(varFound) =>
                  val varType = varFound.getTypeStr
                  obj.asInstanceOf[Identifier].setSymbol(varFound)
                  globalSymbol.lookupClass(varType) match {
                    case Some(value) =>
                      value.lookupMethod(meth.value) match {
                        case Some(methvalue) =>
                          val calledArgsLength: Int = args.length
                          val originalArgsLength: Int = methvalue.argList.length
                          if (calledArgsLength == originalArgsLength) {
                            meth.setSymbol(methvalue)
                          } else {
                            sys.error(
                              "The number of arguments given does not match the number of arguments required for method " + meth.value + "."
                            )
                          }
                        case None =>
                          sys.error(
                            "The method " + meth.value + " has not been declared inside the class ."
                          )
                      }
                    case None =>
                      sys.error(
                        "The variable " + variable + " is not declared in the current scope."
                      )
                  }
              }
            case New(tpe) =>
              val thisClass = globalSymbol.lookupClass(tpe.value)
              thisClass match {
                case Some(classFound) =>
                  tpe.setSymbol(classFound)
                  tpe.setType(TAnyRef(classFound))
                  obj.setType(TAnyRef(classFound))
                  val thisMethod = classFound.lookupMethod(meth.value)
                  thisMethod match {
                    case Some(methodFound) =>
                      val calledArgsLength: Int = args.length
                      val originalArgsLength: Int = methodFound.argList.length
                      if (calledArgsLength == originalArgsLength) {
                        meth.setSymbol(methodFound)
                      } else {
                        sys.error(
                          "The number of arguments given does not match the number of arguments required for method " + meth.value + "."
                        )
                      }
                    case None =>
                      sys.error(
                        "The method " + meth.value + " has not been declared inside the class " + tpe.value + "."
                      )
                  }
                case None =>
                  sys.error("The class " + tpe.value + " does not exist.")
              }
            case MethodCall(innerObj, innerMeth, innerArgs) =>
              var currentLocation = location
              var begin = innerObj
              while (begin.isInstanceOf[MethodCall]) {
                begin = begin.asInstanceOf[MethodCall].obj
              }
              begin match {
                case New(tpe) =>
                  if (globalSymbol.lookupClass(tpe.value) != None) {
                    currentLocation = globalSymbol.lookupClass(tpe.value).get
                  }
                case Identifier(value) =>
                  if (currentLocation
                        .asInstanceOf[MethodSymbol]
                        .lookupVar(value) != None) {
                    val variablesym = currentLocation
                      .asInstanceOf[MethodSymbol]
                      .lookupVar(value)
                      .get
                    val variabletype = currentLocation
                      .asInstanceOf[MethodSymbol]
                      .lookupVar(value)
                      .get
                      .getType
                    begin.asInstanceOf[Identifier].setSymbol(variablesym)
                    if (globalSymbol.lookupClass(variabletype.toString()) != None) {
                      currentLocation =
                        globalSymbol.lookupClass(variabletype.toString()).get
                    }
                  }
                case _ => currentLocation = location
              }
              val thisMethod = currentLocation
                .asInstanceOf[ClassSymbol]
                .lookupMethod(innerMeth.value)
              thisMethod match {
                case Some(methodFound) =>
                  val calledArgsLength: Int = innerArgs.length
                  val originalArgsLength: Int = methodFound.argList.length
                  if (calledArgsLength == originalArgsLength) {
                    innerMeth.setSymbol(methodFound)
                    innerMeth.setType(methodFound.getType)
                    obj.setType(methodFound.getType)
                    for (arg <- innerArgs) {
                      methodExprs(arg, location)
                    }
                  } else {
                    sys.error(
                      "The number of arguments given does not match the number of arguments required for method " + innerMeth.value + "."
                    )
                  }
                case None =>
                  sys.error(
                    "The method " + innerMeth.value + " has not been declared inside the class."
                  )
              }
              methodExprs(meth, currentLocation)
          }
          for (arg <- args) {
            methodExprs(arg, location)
          }
        case Identifier(idName) =>
          if (location.isInstanceOf[MethodSymbol]) {
            val methodsym = location.asInstanceOf[MethodSymbol]
            methodsym.lookupVar(idName) match {
              case Some(varFound) =>
                expr.asInstanceOf[Identifier].setSymbol(varFound)
              case None =>
                globalSymbol.lookupClass(idName) match {
                  case Some(classFound) =>
                    expr.asInstanceOf[Identifier].setSymbol(classFound)
                  case None =>
                    sys.error(
                      idName + " is not declared in the current scope."
                    )
                }
            }
          } else if (location.isInstanceOf[ClassSymbol]) {
            val classSym = location.asInstanceOf[ClassSymbol]
            classSym.lookupMethod(idName) match {
              case Some(methodFound) =>
                expr.asInstanceOf[Identifier].setSymbol(methodFound)

              case None =>
                classSym.lookupVar(idName) match {
                  case Some(varFound) =>
                    expr.asInstanceOf[Identifier].setSymbol(varFound)
                  case None =>
                    globalSymbol.lookupClass(idName) match {
                      case Some(classFound) =>
                        expr.asInstanceOf[Identifier].setSymbol(classFound)
                      case None =>
                        sys.error(
                          idName + " is not declared in the current scope."
                        )
                    }
                }
            }
          } else {
            sys.error("Invalid search location.")
          }
        case If(cond, thn, els) =>
          methodExprs(cond, location)
          methodExprs(thn, location)
          if (els != None) {
            methodExprs(els.get, location)
          }
        case While(cond, body) =>
          methodExprs(cond, location)
          methodExprs(body, location)
        case Println(in) =>
          methodExprs(in, location)
        case Assign(id, rest) =>
          if (location.asInstanceOf[MethodSymbol].params.contains(id.value)) {
            sys.error(
              "Method argument " + id.value + " cannot be reassigned."
            )
          } else {
            methodExprs(id, location)
            methodExprs(rest, location)
          }
        case Block(exprs) =>
          for (in <- exprs) {
            methodExprs(in, location)
          }
      }
    }

    //Global main
    val mainDeclaration = prog.main
    val mainName = mainDeclaration.obj.value
    globalSymbol.mainClass = new ClassSymbol(mainName)
    prog.main.obj.setSymbol(globalSymbol.mainClass)
    prog.main.setSymbol(globalSymbol.mainClass)
    prog.main.obj.setPos(mainDeclaration)
    val parent = prog.main.parent.value
    for (mainVar <- prog.main.vars) {
      val varName: String = mainVar.id.value
      val varSymbol = new VariableSymbol(varName)
      mainVar.setSymbol(varSymbol)
      mainVar.id.setSymbol(varSymbol)
      varSymbol.setPos(mainVar)
      varSymbol.setType(tpeToType(mainVar.tpe))
      if (globalSymbol.mainClass.lookupVar(varName) != None) {
        sys.error(
          "Another variable with the name " + varName + " already exists in the current scope."
        )
      } else {
        mainVar.expr match {
          case Null() | IntLit(_) | StringLit(_) | True() | False() | New(_) =>
            methodExprs(mainVar.expr, globalSymbol.mainClass)
            mainVar.expr.setType(mainVar.expr.getType)
            globalSymbol.mainClass.members = globalSymbol.mainClass.members + (varSymbol.name -> varSymbol)
          case _ =>
            sys.error(
              "Invalid initialization for variable " + varName + "."
            )
        }
      }
    }
    for (expr <- prog.main.exprs) {
      methodExprs(expr, new MethodSymbol(mainName, prog.main.getSymbol))
    }
    prog
  }
}
