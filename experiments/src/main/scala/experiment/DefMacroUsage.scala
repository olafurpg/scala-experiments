package experiment

import java.util
import java.util.Comparator
import java.util.concurrent.atomic.AtomicInteger
import scala.meta._
import scala.util.Sorting

object ImplicitsUsage {
  case class Implicit(string: String)
  def apply(): String = {
    val results = SemanticAnalysis.run { ctx =>
      for {
        d <- ctx.sdatabase.documents
        s <- d.synthetics
        if s.text.indexOf('(') >= 0
        n <- s.names
      } yield n.symbol
    }
    val x = results.flatMap(_._2).groupBy(identity).mapValues(_.size).toArray
    Sorting.quickSort(x)(Ordering.by(-_._2))
    val sb = new java.lang.StringBuilder()
    sb.append("```")
    x.foreach {
      case (symbol, count) =>
        if (count > 10) {
          sb.append(f"$count%10s: ")
            .append(symbol)
            .append('\n')
        }
    }
    sb.append("```")
    sb.toString

  }
}
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
      for {
        document <- ctx.sdatabase.documents
        config = {
          if (document.filename.contains("/test/")) Config.Test
          else Config.Main
        }
        name <- document.names
        if !name.isDefinition
        denot <- ctx.denotation(name.symbol)
        if denot.isMacro
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
            .mapValues(_.size)
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
