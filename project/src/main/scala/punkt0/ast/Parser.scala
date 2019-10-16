package punkt0
package ast

import Trees._
import lexer._

object Parser extends Phase[Iterator[Token], Program] {
  def run(tokens: Iterator[Token])(ctx: Context): Program = {
    import Reporter._

    /** Store the current token, as read from the lexer. */
    var currentToken: Token = new Token(BAD)

    def readToken: Unit = {
      if (tokens.hasNext) {
        // uses nextToken from the Lexer trait
        currentToken = tokens.next
        // skips bad tokens
        while (currentToken.kind == BAD) {
          currentToken = tokens.next
        }
      }
    }

    /** ''Eats'' the expected token, or terminates with an error. */
    def eat(kind: TokenKind): Unit = {
      if (currentToken.kind == kind) {
        readToken
      } else {
        expected(kind)
      }
    }

    /** Complains that what was found was not expected. The method accepts arbitrarily many arguments of type TokenKind */
    def expected(kind: TokenKind, more: TokenKind*): Nothing = {
      fatal(
        "expected: " + (kind :: more.toList)
          .mkString(" or ") + ", found: " + currentToken,
        currentToken
      )
    }

    def identifierParser: Identifier = {
      currentToken.kind match {
        case IDKIND =>
          Identifier(currentToken.asInstanceOf[ID].value).setPos(currentToken)

        case _ => expected(IDKIND)
      }
    }

    def typeParser: TypeTree = {
      val typePos = currentToken
      currentToken.kind match {
        case BOOLEAN =>
          readToken
          BooleanType().setPos(typePos)
        case INT =>
          readToken
          IntType().setPos(typePos)
        case STRING =>
          readToken
          StringType().setPos(typePos)
        case UNIT =>
          readToken
          UnitType().setPos(typePos)
        case IDKIND =>
          val typeID = identifierParser
          readToken
          typeID.setPos(typePos)
        case _ => expected(BOOLEAN, INT, STRING, UNIT, IDKIND)
      }
    }

    // Recursion for operators with regards to their priorities
    def expr0: ExprTree = {
      var toReturn: ExprTree = null
      toReturn = expr1
      while (currentToken.kind.equals(OR)) {
        readToken
        toReturn = Or(toReturn, expr1).setPos(toReturn)
      }
      toReturn
    }

    def expr1: ExprTree = {
      var toReturn: ExprTree = null
      toReturn = expr2
      while (currentToken.kind.equals(AND)) {
        readToken
        toReturn = And(toReturn, expr2).setPos(toReturn)
      }
      toReturn
    }

    def expr2: ExprTree = {
      var toReturn: ExprTree = null
      toReturn = expr3
      while (currentToken.kind.equals(EQUALS) || currentToken.kind.equals(
               LESSTHAN
             )) {
        if (currentToken.kind.equals(EQUALS)) {
          readToken
          toReturn = Equals(toReturn, expr3).setPos(toReturn)
        } else {
          readToken
          toReturn = LessThan(toReturn, expr3).setPos(toReturn)
        }
      }
      toReturn
    }

    def expr3: ExprTree = {
      var toReturn: ExprTree = null
      toReturn = expr4
      while (currentToken.kind.equals(PLUS) || currentToken.kind.equals(MINUS)) {
        if (currentToken.kind.equals(PLUS)) {
          readToken
          toReturn = Plus(toReturn, expr4).setPos(toReturn)
        } else {
          readToken
          toReturn = Minus(toReturn, expr4).setPos(toReturn)
        }
      }
      toReturn
    }

    def expr4: ExprTree = {
      var toReturn: ExprTree = null
      toReturn = exprParser
      while (currentToken.kind.equals(TIMES) || currentToken.kind.equals(DIV)) {
        if (currentToken.kind.equals(TIMES)) {
          readToken
          toReturn = Times(toReturn, exprParser).setPos(toReturn)
        } else {
          readToken
          toReturn = Div(toReturn, exprParser).setPos(toReturn)
        }
      }
      toReturn
    }

    def exprParser: ExprTree = {
      var toReturn: ExprTree = null
      currentToken.kind match {
        case TRUE => {
          val truePos = currentToken
          readToken
          toReturn = True().setPos(truePos)
        }
        case FALSE =>
          val falsePos = currentToken
          readToken
          toReturn = False().setPos(falsePos)
        case THIS =>
          val thisPos = currentToken
          readToken
          toReturn = This().setPos(thisPos)
        case NULL =>
          val nullPos = currentToken
          readToken
          toReturn = Null().setPos(nullPos)
        case INTLITKIND =>
          val intPos = currentToken
          readToken
          toReturn = IntLit(intPos.asInstanceOf[INTLIT].value).setPos(intPos)
        case STRLITKIND =>
          val strPos = currentToken
          readToken
          toReturn = StringLit(strPos.asInstanceOf[STRLIT].value).setPos(strPos)
        case IDKIND =>
          val idPos = currentToken
          val ident = identifierParser.setPos(idPos)
          readToken
          currentToken.kind match {
            case EQSIGN =>
              readToken
              val expression = expr0
              toReturn = Assign(ident, expression).setPos(idPos)
            case _ =>
              toReturn = ident
          }
        case NEW =>
          val newPos = currentToken
          readToken
          currentToken.kind match {
            case IDKIND =>
              val ident = identifierParser
              readToken
              eat(LPAREN)
              eat(RPAREN)
              toReturn = New(ident).setPos(newPos)
            case _ => expected(IDKIND)
          }
        case BANG =>
          val notPos = currentToken
          readToken
          val expression = exprParser
          toReturn = Not(expression).setPos(notPos)
        case LPAREN =>
          val lParenPos = currentToken
          readToken
          toReturn = expr0.setPos(lParenPos)
          eat(RPAREN)
        // statements
        case LBRACE =>
          val blockPos = currentToken
          readToken
          var expressions = List[ExprTree]()
          if (isAnExpression(currentToken)) {
            expressions = expressions ++ List(expr0)
            while (currentToken.kind.equals(SEMICOLON)) {
              readToken
              expressions = expressions ++ List(expr0)
            }
          }
          eat(RBRACE)
          toReturn = Block(expressions).setPos(blockPos)
        case IF =>
          val ifPos = currentToken
          readToken
          eat(LPAREN)
          val condition: ExprTree = expr0
          eat(RPAREN)
          val trueBranch: ExprTree = expr0
          var elseBranch: Option[ExprTree] = None
          if (currentToken.kind.equals(ELSE)) {
            readToken
            elseBranch = Some(expr0)
          }
          toReturn = If(condition, trueBranch, elseBranch).setPos(ifPos)
        case WHILE =>
          val whilePos = currentToken
          readToken
          eat(LPAREN)
          val condition: ExprTree = expr0
          eat(RPAREN)
          val body = expr0
          toReturn = While(condition, body).setPos(whilePos)
        case PRINTLN =>
          val printPos = currentToken
          readToken
          eat(LPAREN)
          val expression = expr0
          eat(RPAREN)
          toReturn = Println(expression).setPos(printPos)
        case _ =>
          expected(
            TRUE,
            FALSE,
            THIS,
            NULL,
            INTLITKIND,
            STRLITKIND,
            IDKIND,
            NEW,
            BANG,
            LPAREN,
            LBRACE,
            IF,
            WHILE,
            PRINTLN
          )
      }
      recursiveExprParser(toReturn)
    }

    def recursiveExprParser(expression: ExprTree): ExprTree = {
      val dotPos = currentToken
      var toReturn: ExprTree = null
      currentToken.kind match {
        case DOT =>
          readToken
          currentToken.kind match {
            case IDKIND =>
              val ident = identifierParser
              readToken
              eat(LPAREN)
              var args = List[ExprTree]()
              if (isAnExpression(currentToken)) {
                args = args ++ List(expr0)
                while (currentToken.kind.equals(COMMA)) {
                  readToken
                  args = args ++ List(expr0)
                }
              }
              eat(RPAREN)
              toReturn = MethodCall(expression, ident, args).setPos(dotPos)
            case _ => expected(IDKIND)
          }
          recursiveExprParser(toReturn)
        case _ => expression
      }
    }

    def varDeclParser: VarDecl = {
      val varPos = currentToken
      eat(VAR)
      val varID = identifierParser
      readToken
      eat(COLON)
      val varType = typeParser
      eat(EQSIGN)
      val expression = expr0
      eat(SEMICOLON)
      VarDecl(varType, varID, expression).setPos(varPos)
    }

    def formalParser: Formal = {
      val formalPos = currentToken
      val formalID = identifierParser
      readToken
      eat(COLON)
      val formalType = typeParser
      Formal(formalType, formalID).setPos(formalPos)
    }

    def methodDeclParser: MethodDecl = {
      val methodPos = currentToken
      var overrideVar: Boolean = false
      if (currentToken.kind.equals(OVERRIDE)) {
        overrideVar = true
        readToken
      }
      eat(DEF)
      val methodID = identifierParser
      readToken
      eat(LPAREN)
      var args = List[Formal]()
      if (currentToken.kind.equals(IDKIND)) {
        args = args ++ List(formalParser)
        while (currentToken.kind.equals(COMMA)) {
          readToken
          args = args ++ List(formalParser)
        }
      }
      eat(RPAREN)
      eat(COLON)
      val methodType = typeParser
      eat(EQSIGN)
      eat(LBRACE)
      var vars = List[VarDecl]()
      var expressions = List[ExprTree]()
      var returnExpr: ExprTree = null
      while (currentToken.kind.equals(VAR)) {
        vars = vars ++ List(varDeclParser)
      }
      val expression1 = expr0
      expressions = expressions ++ List(expression1)
      while (currentToken.kind.equals(SEMICOLON)) {
        readToken
        expressions = expressions ++ List(expr0)
      }
      returnExpr = expressions.last
      expressions = expressions.dropRight(1)
      eat(RBRACE)
      MethodDecl(
        overrideVar,
        methodType,
        methodID,
        args,
        vars,
        expressions,
        returnExpr
      ).setPos(methodPos)
    }

    def mainDeclParser: MainDecl = {
      val mainPos = currentToken
      eat(OBJECT)
      val mainObject = identifierParser
      readToken
      eat(EXTENDS)
      val parent = identifierParser
      readToken
      eat(LBRACE)
      var vars = List[VarDecl]()
      while (currentToken.kind.equals(VAR)) {
        vars = vars ++ List(varDeclParser)
      }
      var expressions = List[ExprTree]()
      val expression1 = expr0
      expressions = expressions ++ List(expression1)
      while (currentToken.kind.equals(SEMICOLON)) {
        readToken
        expressions = expressions ++ List(expr0)
      }
      eat(RBRACE)
      MainDecl(mainObject, parent, vars, expressions).setPos(mainPos)
    }

    def classDeclParser: ClassDecl = {
      val classPos = currentToken
      eat(CLASS)
      val classID = identifierParser
      readToken
      var classParent: Option[Identifier] = None
      if (currentToken.kind.equals(EXTENDS)) {
        readToken
        classParent = Option(identifierParser)
        readToken
      }
      eat(LBRACE)
      var vars = List[VarDecl]()
      while (currentToken.kind.equals(VAR)) {
        vars = vars ++ List(varDeclParser)
      }
      var methodDeclarations = List[MethodDecl]()
      while (currentToken.kind.equals(OVERRIDE) || currentToken.kind.equals(
               DEF
             )) {
        methodDeclarations = methodDeclarations ++ List(methodDeclParser)
      }
      eat(RBRACE)
      ClassDecl(classID, classParent, vars, methodDeclarations).setPos(classPos)
    }

    def parseGoal: Program = {
      val programPos = currentToken
      var classes = List[ClassDecl]()
      while (currentToken.kind.equals(CLASS)) {
        classes = classes ++ List(classDeclParser)
      }
      val programMain = mainDeclParser
      eat(EOF)
      Program(programMain, classes).setPos(programPos)
    }

    readToken
    val tree = parseGoal
    terminateIfErrors
    tree
  }

  def isAnExpression(token: Token): Boolean = {
    if (token.kind.equals(INTLITKIND) || token.kind.equals(STRLITKIND) || token.kind
          .equals(TRUE) || token.kind.equals(FALSE) || token.kind.equals(IDKIND)
        || token.kind.equals(THIS) || token.kind.equals(NULL) || token.kind
          .equals(NEW) || token.kind.equals(BANG) || token.kind.equals(LPAREN) || token.kind
          .equals(LBRACE) || token.kind.equals(IF) || token.kind
          .equals(WHILE) || token.kind.equals(PRINTLN)) {
      return true
    } else {
      return false
    }
  }
}