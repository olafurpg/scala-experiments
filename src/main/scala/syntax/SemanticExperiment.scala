package syntax

import java.nio.file.Path
import scala.collection.mutable
import scala.meta._

sealed trait Config
object Config {
  case object Test extends Config
  case object Main extends Config
  case object Unknown extends Config
}

case class MacroCall(symbol: Symbol, name: Name, config: Config)

object Driver extends App {
  private val rootPath = "target/semanticdb.v11"

  val root = AbsolutePath(rootPath)

    val results: mutable.Seq[(Path, Seq[MacroCall])] = SemanticAnalysis.run(root) { ctx =>
      val config: Config = ctx.database.documents.head.input match {
        case Input.VirtualFile(path, _) =>
          if (path.contains("/test/")) Config.Test
          else Config.Main
        case _ => Config.Unknown
      }
      ctx.tree.collect {
        case n: Name if ctx.symbol(n.pos).exists(x => ctx.denotation(x).exists(_.isMacro)) =>
          MacroCall(ctx.symbol(n.pos).get, n, config)
      }
    }

    results
      .flatMap(_._2)
      .groupBy(_.config)
      .foreach {
        case (config, calls) =>
          println(s"#==========================")
          println(s"#== $config")
          println(s"#==========================")
          calls
            .groupBy(_.symbol)
            .mapValues(_.length)
            .toSeq
            .sortBy(_._2)
            .foreach {
              case (symbol, count) =>
                println(f"$count%10s: $symbol")
            }
      }
}