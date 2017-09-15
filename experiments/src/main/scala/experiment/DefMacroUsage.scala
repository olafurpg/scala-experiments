package experiment

import scala.meta._

object DefMacroUsage {
  sealed trait Config
  object Config {
    case object Test extends Config
    case object Main extends Config
    case object Unknown extends Config
  }
  case class MacroCall(symbol: Symbol, config: Config)
  def apply(): String = {
    val results = SemanticAnalysis.run { ctx =>
      // val config: Config = ctx.database.documents.head.input match {
      //   case Input.VirtualFile(path, _) =>
      //     if (path.contains("/test/")) Config.Test
      //     else Config.Main
      //   case _ => Config.Unknown
      // }
      // for {
      //   name <- ctx.database.names
      //   if !name.isDefinition
      //   denot <- ctx.denotation(name.symbol)
      //   if denot.isMacro
      // } yield MacroCall(name.symbol, config)
      ctx.document.contents.lines.length :: Nil

    }

    val sb = new StringBuilder
    def println(s: String) = {
      sb ++= s
      sb ++= "\n"
    }

    // results
    //   .flatMap(_._2)
    //   .groupBy(_.config)
    //   .foreach {
    //     case (config, calls) =>
    //       println(s"# $config")
    //       println("```")
    //       calls
    //         .groupBy(_.symbol)
    //         .mapValues(_.length)
    //         .toSeq
    //         .sortBy(-_._2)
    //         .foreach {
    //           case (symbol, count) =>
    //             if (count > 5) {
    //               println(f"$count%10s: $symbol")
    //             }
    //         }
    //       println("```")
    //   }
    val sum = results.map {
      case (_, counts) => counts.head
    }.sum
    System.out.println(sum)
    sb.toString()
  }
}
