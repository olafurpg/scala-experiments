package experiment

import scala.meta._

object DefMacroUsage {
  sealed trait Config
  object Config {
    case object Test extends Config
    case object Main extends Config
    case object Unknown extends Config
  }
  case class MacroCall(symbol: String, config: Config)
  def apply(): String = {
    val results = SemanticAnalysis.run { ctx =>
      val path = ctx.sdatabase.documents.head.filename
      val config: Config =
        if (path.contains("/test/")) Config.Test
        else Config.Main
      for {
        document <- ctx.sdatabase.documents
        name <- document.names
        if !name.isDefinition
        denot <- ctx.denotation(name.symbol)
        // aa = (
        //   else ()
        // )
        if {
          if (denot.name == "reify") {
            Predef.println("target/semanticdb.v11/" + document.filename)
          }
          denot.isMacro
        }
      } yield MacroCall(name.symbol, config)
    }

    val sb = new StringBuilder
    def println(s: String) = {
      sb ++= s
      sb ++= "\n"
    }

    results
      .flatMap(_._2)
      .groupBy(_.config)
      .foreach {
        case (config, calls) =>
          println(s"# $config")
          println("```")
          calls
            .groupBy(_.symbol)
            .mapValues(_.length)
            .toSeq
            .sortBy(-_._2)
            .foreach {
              case (symbol, count) =>
                if (count > 5) {
                  println(f"$count%10s: $symbol")
                }
            }
          println("```")
      }
    sb.toString()
  }
}
