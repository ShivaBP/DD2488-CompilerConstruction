package punkt0

import java.io.File

import lexer._

object Main {

  def processOptions(args: Array[String]): Context = {
    var ctx = Context()

    def processOption(args: List[String]): Unit = args match {
      case "--help" :: args =>
        ctx = ctx.copy(doHelp = true)
        processOption(args)

      case "-d" :: out :: args =>
        ctx = ctx.copy(outDir = Some(new File(out)))
        processOption(args)

      case "--tokens" :: args =>
        ctx = ctx.copy(doTokens = true)
        processOption(args)

      case f :: args =>
        ctx = ctx.copy(file = Some(new File(f)))
        processOption(args)

      case List() =>
    }

    processOption(args.toList)

    if (ctx.doHelp) {
      displayHelp()
      sys.exit(0)
    }

    if (ctx.doTokens) {
      displayTokens(ctx)
    }
    ctx
  }

  def displayTokens(ctx: Context): Unit = {
    val lexerIter = Lexer.run(ctx.file.get)(ctx)
    while (lexerIter.hasNext) {
      var t = lexerIter.next()
      if (t != BAD) {
        println(t + "(" + t.line + ":" + t.column + ")")
      }
    }
  }
  
  def displayHelp(): Unit = {
    println("Usage: <punkt0c> [options] <file>")
    println("Options include:")
    println(" --help        displays this help")
    println(" -d <outdir>   generates class files in the specified directory")
  }

  def main(args: Array[String]): Unit = {
    val ctx = processOptions(args)
    val lexerResult = Lexer.run(ctx.file.get)(ctx)
  }
}