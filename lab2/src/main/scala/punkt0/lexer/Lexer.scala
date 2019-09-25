package punkt0
package lexer

import java.io.File
import scala.collection.mutable

object Lexer extends Phase[File, Iterator[Token]] {
  import Reporter._

  def run(f: File)(ctx: Context): Iterator[Token] = {
    val source = scala.io.Source.fromFile(f)
    val EOFCharacter = -1.toChar
    var fileEnd: Boolean = false
    var hasSentEOF: Boolean = false
    var current = source.next()

    new Iterator[Token] {

      def hasNext: Boolean = {
        if (fileEnd && hasSentEOF) {
          return false
        } else {
          return true
        }
      }

      def next: Token = {
        var tokenToReturn: Token = null
        var currentPosition = source.pos

        // Handle EOF
        if (fileEnd && !hasSentEOF) {
          hasSentEOF = true
          currentPosition = source.pos
          var eofToken: Token = new Token(EOF)
          eofToken.setPos(f, currentPosition + 1)
          return eofToken
        }

        // Handle tokens
        currentPosition = source.pos

        // Forward slash
        if (current.equals('/')) {
          if (source.hasNext) {
            current = source.next()
            current match {
              case '/' => {
                while (!current.equals('\n') && source.hasNext) {
                  current = source.next()
                }
                if (!source.hasNext) {
                  fileEnd = true
                  new Token(EOF)
                } else {
                  current = source.next()
                }
              }
              case '*' => {
                if (source.hasNext) {
                  current = source.next()
                  if (current.equals('*')) {
                    if (source.hasNext) {
                      current = source.next()
                      if (current.equals('/')) {
                        if (source.hasNext) {
                          current = source.next()
                        } else {
                          fileEnd = true
                          new Token(EOF)
                        }
                      } else if (!current.equals('/') && !source.hasNext) {
                        fileEnd = true
                        Reporter.error("Invalid Block comment ")
                        Reporter.terminateIfErrors()
                        new Token(BAD)
                      } else {
                        current = source.next()
                      }

                    } else {
                      fileEnd = true
                      Reporter.error("Invalid Block comment ")
                      Reporter.terminateIfErrors()
                      new Token(BAD)
                    }
                  } else if (!current.equals('/') && !source.hasNext) {
                    fileEnd = true
                    Reporter.error("Invalid token ")
                    Reporter.terminateIfErrors()
                    new Token(BAD)
                  } else {
                    current = source.next()
                  }

                } else {
                  fileEnd = true
                  Reporter.error("Invalid Block comment ")
                  Reporter.terminateIfErrors()
                  new Token(BAD)
                }
              }
              case _ => {
                Reporter.error("Invalid token ")
                Reporter.terminateIfErrors()
                new Token(BAD)
              }
            }
          } else {
            // Division sign
            new Token(DIV)
          }
        }

        //Keyword or identifier
        else if (isLetter(ascii(current))) {
          var tempStorage = new StringBuilder()
          tempStorage += current
          if (source.hasNext) {
            current = source.next()
          } else {
            fileEnd = true
          }
          while (source.hasNext && (isLetter(ascii(current)) || isDigit(
                   ascii(current)
                 ) || (current.equals('_')))) {
            tempStorage += current
            current = source.next()
          }
          if (!source.hasNext) {
            tempStorage += current
            fileEnd = true
          }
          var wannaBeToken = tempStorage.toString()
          tokenToReturn = wannaBeToken match {
            // Keyword
            case "Boolean"  => new Token(BOOLEAN)
            case "class"    => new Token(CLASS)
            case "def"      => new Token(DEF)
            case "else"     => new Token(ELSE)
            case "extends"  => new Token(EXTENDS)
            case "false"    => new Token(FALSE)
            case "if"       => new Token(IF)
            case "Int"      => new Token(INT)
            case "new"      => new Token(NEW)
            case "null"     => new Token(NULL)
            case "object"   => new Token(OBJECT)
            case "override" => new Token(OVERRIDE)
            case "println"  => new Token(PRINTLN)
            case "String"   => new Token(STRING)
            case "this"     => new Token(THIS)
            case "true"     => new Token(TRUE)
            case "Unit"     => new Token(UNIT)
            case "var"      => new Token(VAR)
            case "while"    => new Token(WHILE)
            // Identifier
            case _ => new ID(wannaBeToken)
          }
        }

        //Integer literal
        else if (isDigit(ascii(current))) {
          while (current.equals('0')) {
            if (source.hasNext) {
              current = source.next()
            } else {
              fileEnd = true
            }
          }
          var k = 0
          if (!current.equals('0')) {
            while (isDigit(ascii(current))) {
              k = 10 * k + current.toString().toInt
              if (source.hasNext) {
                current = source.next()
              } else {
                fileEnd = true
              }
            }
          }
          tokenToReturn = new INTLIT(k)
          if (source.hasNext) {
            current = source.next()
          } else {
            fileEnd = true
          }
        }

        // String literals
        else if (current.equals('"')) {
          if (source.hasNext) {
            current = source.next()
          }
          var tempStorage = new StringBuilder()
          while (source.hasNext && !current.equals('"') && !current.equals(
                   '\n'
                 )) {
            tempStorage += current
            current = source.next()
          }
          if (current.equals('"')) {
            var wannaBeToken = tempStorage.toString()
            tokenToReturn = new STRLIT(wannaBeToken)

          } else {
            Reporter.error("Invalid String Literal")
            Reporter.terminateIfErrors()
            tokenToReturn = new Token(BAD)
          }
          if (source.hasNext) {
            current = source.next()
          } else {
            fileEnd = true
          }
        }

        // Special characters
        else {
          tokenToReturn = current match {
            case ':' => new Token(COLON)
            case ';' => new Token(SEMICOLON)
            case '.' => new Token(DOT)
            case ',' => new Token(COMMA)
            case '!' => new Token(BANG)
            case '<' => new Token(LESSTHAN)
            case '+' => new Token(PLUS)
            case '-' => new Token(MINUS)
            case '*' => new Token(TIMES)
            case '{' => new Token(LBRACE)
            case '}' => new Token(RBRACE)
            case '(' => new Token(LPAREN)
            case ')' => new Token(RPAREN)
            case '=' => {
              if (source.hasNext && source.next().equals('=')) {
                new Token(EQUALS)
              } else {
                new Token(EQSIGN)
              }
            }
            case '&' => {
              if (source.hasNext && source.next().equals('&')) {
                new Token(AND)
              } else {
                Reporter.error("Invalid AND")
                Reporter.terminateIfErrors()
                new Token(BAD)
              }
            }
            case '|' => {
              if (source.hasNext && source.next().equals('|')) {
                new Token(OR)
              } else {
                Reporter.error("Invalid OR")
                Reporter.terminateIfErrors()
                new Token(BAD)
              }
            }
            case EOFCharacter => new Token(EOF)
            case _ =>
              Reporter.error("Invalid token")
              Reporter.terminateIfErrors()
              new Token(BAD)
          }
          if (source.hasNext) {
            current = source.next()
          } else {
            fileEnd = true
          }
        }

        //Whitespace
        while (source.hasNext && (current.isWhitespace || current.equals('\n') || current
                 .equals(' '))) {
          current = source.next()
        }

        // EOF
        if (!source.hasNext) {
          fileEnd = true
        }

        // The token
        tokenToReturn.setPos(f, currentPosition)
        tokenToReturn
      }
    }
  }

  def isLetter(input: Int): Boolean = {
    if ((input >= 65 && input <= 90) || (input >= 97 && input <= 122)) {
      return true
    }
    return false
  }
  def isDigit(input: Int): Boolean = {
    if (input >= 48 && input <= 57) {
      return true
    }
    return false
  }

  def ascii(character: Char): Int = {
    var asciiCode = character.toInt
    return asciiCode
  }
}
