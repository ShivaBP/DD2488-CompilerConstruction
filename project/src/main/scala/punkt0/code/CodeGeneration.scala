package punkt0
package code

import ast.Trees._
import analyzer.Symbols._
import analyzer.Types._
import cafebabe._
import AbstractByteCodes.{New => _, _}
import ByteCodes._

object CodeGeneration extends Phase[Program, Unit] {
  var slots = Map[Int, Int]()

  def recursivePOP(ch: CodeHandler, leftOnStack: Int) =
    for (i <- 1 to leftOnStack) {
      ch << POP
    }

  def typeConvert(tpe: Type): String = {
    tpe match {
      case TAnyRef(classSymbol) => "L" + classSymbol.name + ";"
      case TBoolean             => "Z"
      case TInt                 => "I"
      case TString              => "Ljava/lang/String;"
      case TObject(classSymbol) => "L" + classSymbol.name + ";"
      case TUnit                => "V"

    }
  }

  def exprHandler(
      locationClass: ClassSymbol,
      locationMeth: MethodSymbol,
      ch: CodeHandler,
      expr: ExprTree
  ): Int = {
    var numberToPop: Int = 0
    expr match {
      case Null() =>
        ch << ACONST_NULL
        numberToPop = 1
      case This() =>
        ch << ArgLoad(0)
        numberToPop = 1
      case True() =>
        ch << ICONST_1
        numberToPop = 1
      case False() =>
        ch << ICONST_0
        numberToPop = 1
      case IntLit(i) =>
        ch << Ldc(i)
        numberToPop = 1
      case StringLit(s) =>
        ch << Ldc(s)
        numberToPop = 1
      case New(tpe) =>
        ch << DefaultNew(tpe.getSymbol.asInstanceOf[ClassSymbol].name)
        numberToPop = 1
      case Identifier(value) =>
        val variRetreived =
          expr.asInstanceOf[Identifier].getSymbol.asInstanceOf[VariableSymbol]
        if (locationMeth.argList.contains(variRetreived) && locationMeth != null) {
          var varIndex = locationMeth.argList.indexOf(variRetreived)
          ch << ArgLoad(varIndex + 1)
        } else if (slots.get(variRetreived.id) != None) {
          variRetreived.getType match {
            case TInt | TBoolean =>
              ch << ILoad(slots.get(variRetreived.id).get)
            case TObject(_) | TString =>
              ch << ALoad(slots.get(variRetreived.id).get)
          }
        } else {
          ch << ArgLoad(0)
          ch << GetField(
            locationClass.name,
            variRetreived.name,
            typeConvert(variRetreived.getType)
          )
        }
        numberToPop = 1
      case Not(expr) =>
        ch << ICONST_1
        exprHandler(locationClass, locationMeth, ch, expr)
        ch << ISUB
        numberToPop = 1
      case Times(lhs, rhs) =>
        exprHandler(locationClass, locationMeth, ch, lhs)
        exprHandler(locationClass, locationMeth, ch, rhs)
        ch << IMUL
        numberToPop = 1
      case Div(lhs, rhs) =>
        exprHandler(locationClass, locationMeth, ch, lhs)
        exprHandler(locationClass, locationMeth, ch, rhs)
        ch << IDIV
        numberToPop = 1
      case Minus(lhs, rhs) =>
        exprHandler(locationClass, locationMeth, ch, lhs)
        exprHandler(locationClass, locationMeth, ch, rhs)
        ch << ISUB
        numberToPop = 1
      case Plus(lhs, rhs) =>
        (lhs.getType, rhs.getType) match {
          case (TInt, TInt) =>
            exprHandler(locationClass, locationMeth, ch, lhs)
            exprHandler(locationClass, locationMeth, ch, rhs)
            ch << IADD
            numberToPop = 1
          case _ =>
            ch << DefaultNew("java/lang/StringBuilder")
            exprHandler(locationClass, locationMeth, ch, lhs)
            ch << InvokeVirtual(
              "java/lang/StringBuilder",
              "append",
              "(" + typeConvert(lhs.getType) + ")Ljava/lang/StringBuilder;"
            )
            exprHandler(locationClass, locationMeth, ch, rhs)
            ch << InvokeVirtual(
              "java/lang/StringBuilder",
              "append",
              "(" + typeConvert(rhs.getType) + ")Ljava/lang/StringBuilder;"
            )
            ch << InvokeVirtual(
              "java/lang/StringBuilder",
              "toString",
              "()Ljava/lang/String;"
            )
            numberToPop = 1
        }
      case And(lhs, rhs) =>
        (lhs.getType, rhs.getType) match {
          case (TBoolean, TBoolean) =>
            val nAfter = ch.getFreshLabel("nAfter");
            val nElse = ch.getFreshLabel("nElse")
            exprHandler(locationClass, locationMeth, ch, lhs)
            ch << IfEq(nElse)
            exprHandler(locationClass, locationMeth, ch, rhs)
            ch << Goto(nAfter)
            ch << Label(nElse)
            ch << ICONST_0
            ch << Label(nAfter)
            numberToPop = 1
        }
      case Or(lhs, rhs) =>
        (lhs.getType, rhs.getType) match {
          case (TBoolean, TBoolean) =>
            val nAfter = ch.getFreshLabel("nAfter");
            val nElse = ch.getFreshLabel("nElse")
            exprHandler(locationClass, locationMeth, ch, lhs)
            ch << IfEq(nElse)
            ch << ICONST_1
            ch << Goto(nAfter)
            ch << Label(nElse)
            exprHandler(locationClass, locationMeth, ch, rhs)
            ch << Label(nAfter)
            numberToPop = 1
        }
      case LessThan(lhs, rhs) =>
        val toReturn = ch.getFreshLabel("Return")
        val returnTrue = ch.getFreshLabel("True")
        exprHandler(locationClass, locationMeth, ch, lhs)
        exprHandler(locationClass, locationMeth, ch, rhs)
        ch << If_ICmpLt(returnTrue)
        ch << ICONST_0
        ch << Goto(toReturn)
        ch << Label(returnTrue)
        ch << ICONST_1
        ch << Label(toReturn)
        numberToPop = 1
      case Equals(lhs, rhs) =>
        lhs.getType match {
          case TInt | TBoolean =>
            val nTrue = ch.getFreshLabel("nTrue")
            val nAfter = ch.getFreshLabel("nAfter")
            exprHandler(locationClass, locationMeth, ch, lhs)
            exprHandler(locationClass, locationMeth, ch, rhs)
            ch << If_ICmpEq(nTrue)
            ch << ICONST_0
            ch << Goto(nAfter)
            ch << Label(nTrue)
            ch << ICONST_1
            ch << Label(nAfter)
          case _ =>
            val nTrue = ch.getFreshLabel("nTrue")
            val nAfter = ch.getFreshLabel("nAfter")
            exprHandler(locationClass, locationMeth, ch, lhs)
            exprHandler(locationClass, locationMeth, ch, rhs)
            ch << If_ACmpEq(nTrue)
            ch << ICONST_0
            ch << Goto(nAfter)
            ch << Label(nTrue)
            ch << ICONST_1
            ch << Label(nAfter)
        }
        numberToPop = 1
      case MethodCall(obj, meth, args) =>
        exprHandler(locationClass, locationMeth, ch, obj)
        var methSignature = "("
        for (arg <- args) {
          exprHandler(locationClass, locationMeth, ch, arg)
          methSignature = methSignature + typeConvert(arg.getType)
        }
        methSignature = methSignature + ")" + typeConvert(
          meth.getSymbol.getType
        )
        ch << InvokeVirtual(
          obj.getType.asInstanceOf[TObject].classSymbol.name,
          meth.value,
          methSignature
        )
        if (meth.getSymbol.getType == TUnit)
          numberToPop = 0
        else
          numberToPop = 1
      case Println(expr) =>
        ch << GetStatic("java/lang/System", "out", "Ljava/io/PrintStream;")
        exprHandler(locationClass, locationMeth, ch, expr)
        ch << InvokeVirtual(
          "java/io/PrintStream",
          "println",
          "(" + typeConvert(expr.getType) + ")V"
        )
        numberToPop = 0
      case Block(exprs) =>
        if (exprs.length > 0) {
          exprHandler(locationClass, locationMeth, ch, exprs.head)
          if (exprs.tail.length != 0) {
            var rest = exprs.tail
            while (rest.length > 1) {
              recursivePOP(
                ch,
                exprHandler(locationClass, locationMeth, ch, rest.head)
              )
              rest = rest.tail
            }
            exprHandler(locationClass, locationMeth, ch, rest.head)
          }
        }
        if (expr.getType == TUnit)
          numberToPop = 0
        else
          numberToPop = 1
      case If(ifExpr, thn, Some(els)) =>
        val nElse = ch.getFreshLabel("nElse")
        val nAfter = ch.getFreshLabel("nAfter")
        exprHandler(locationClass, locationMeth, ch, ifExpr)
        ch << IfEq(nElse)
        exprHandler(locationClass, locationMeth, ch, thn)
        ch << Goto(nAfter)
        ch << Label(nElse)
        exprHandler(locationClass, locationMeth, ch, els)
        ch << Label(nAfter)
        if (expr.getType == TUnit) {
          numberToPop = 0
        } else {
          numberToPop = 1
        }
      case If(expr, thn, None) =>
        val nAfter = ch.getFreshLabel("nAfter")
        exprHandler(locationClass, locationMeth, ch, expr)
        ch << IfEq(nAfter)
        exprHandler(locationClass, locationMeth, ch, thn)
        ch << Label(nAfter)
        numberToPop = 0
      case While(cond, body) =>
        val nStart = ch.getFreshLabel("nStart");
        val nExit = ch.getFreshLabel("nExit")
        ch << Label(nStart)
        exprHandler(locationClass, locationMeth, ch, cond)
        ch << IfEq(nExit)
        exprHandler(locationClass, locationMeth, ch, body)
        ch << Goto(nStart)
        ch << Label(nExit)
        numberToPop = 0
      case Assign(id, expr) =>
        val variableRetreived = id.getSymbol.asInstanceOf[VariableSymbol]
        if (slots.get(variableRetreived.id) != None) {
          exprHandler(locationClass, locationMeth, ch, expr)
          variableRetreived.getType match {
            case TInt | TBoolean =>
              ch << IStore(slots.get(variableRetreived.id).get)

            case TObject(_) | TString =>
              ch << AStore(slots.get(variableRetreived.id).get)
          }
        } else {
          ch << ArgLoad(0)
          exprHandler(locationClass, locationMeth, ch, expr)
          ch << PutField(
            locationClass.name,
            id.value,
            typeConvert(variableRetreived.getType)
          )
        }
        numberToPop = 0
    }
    numberToPop
  }

  def run(prog: Program)(ctx: Context): Unit = {
    // Class
    def generateClassFile(
        sourceName: String,
        ct: ClassDecl,
        dir: String
    ): Unit = {
      var superparent: Option[String] = None
      if (ct.parent != None) {
        superparent = Some(ct.parent.get.value)
      }
      val classFile = new ClassFile(
        ct.id.value,
        superparent
      )
      classFile.setSourceFile(sourceName)
      for (classVar <- ct.vars) {
        classFile.addField(
          typeConvert(classVar.getSymbol.getType),
          classVar.id.value
        )
      }
      classFile.addDefaultConstructor
      for (classMeth <- ct.methods) {
        var argString = ""
        for (arg <- classMeth.args) {
          argString = argString + typeConvert(arg.getSymbol.getType)
        }
        var method = classFile
          .addMethod(
            typeConvert(classMeth.getSymbol.getType),
            classMeth.id.value,
            argString
          )
          .codeHandler
        generateMethodCode(method, classMeth)
      }
      classFile.writeToFile(dir + ct.id.value + ".class")
    }

    // Method
    def generateMethodCode(ch: CodeHandler, mt: MethodDecl): Unit = {
      for (methVar <- mt.vars) {
        exprHandler(
          mt.getSymbol.classSymbol,
          mt.getSymbol,
          ch,
          methVar.expr
        )
        slots = slots + (methVar.getSymbol.id -> ch.getFreshVar)
        methVar.getSymbol.getType match {
          case TInt | TBoolean =>
            ch << IStore(slots.get(methVar.getSymbol.id).get)
          case TObject(_) | TString =>
            ch << AStore(slots.get(methVar.getSymbol.id).get)
        }
      }
      for (expr <- mt.exprs) {
        recursivePOP(
          ch,
          exprHandler(mt.getSymbol.classSymbol, mt.getSymbol, ch, expr)
        )
      }
      exprHandler(mt.getSymbol.classSymbol, mt.getSymbol, ch, mt.retExpr)
      mt.getSymbol.getType match {
        case TInt | TBoolean =>
          ch << IRETURN
        case TObject(_) | TString =>
          ch << ARETURN
        case TUnit =>
          ch << RETURN
      }
      ch.freeze
    }

    // Output
    val outDir = ctx.outDir.map(_.getPath + "/").getOrElse("./")
    val f = new java.io.File(outDir)
    if (!f.exists()) {
      f.mkdir()
    }
    val sourceName = ctx.file.get.getName
    prog.classes foreach { ct =>
      generateClassFile(sourceName, ct, outDir)
    }

    // Main
    val mainObj = new ClassFile(prog.main.obj.value, None)
    mainObj.addDefaultConstructor
    val mainch = mainObj.addMainMethod.codeHandler
    for (mainVar <- prog.main.vars) {
      exprHandler(
        prog.main.getSymbol,
        null,
        mainch,
        mainVar.expr
      )
      slots = slots + (mainVar.getSymbol.id -> mainch.getFreshVar)
      mainVar.getSymbol.getType match {
        case TInt | TBoolean =>
          mainch << IStore(slots.get(mainVar.getSymbol.id).get)
        case TObject(_) | TString =>
          mainch << AStore(slots.get(mainVar.getSymbol.id).get)
      }
    }
    for (expr <- prog.main.exprs) {
      recursivePOP(
        mainch,
        exprHandler(
          prog.main.getSymbol,
          null,
          mainch,
          expr
        )
      )
    }
    mainch << RETURN
    mainch.freeze
    mainObj.writeToFile(outDir + prog.main.obj.value + ".class")
  }
}