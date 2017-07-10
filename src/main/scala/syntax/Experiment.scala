package syntax

import java.nio.file.Files
import java.nio.file.Paths
import scala.meta._
import testkit._

object Experiment {

  def symbolicInfix(corpus: Corpus): String = {
    val files = Corpus.files(corpus).toBuffer.par
    val results =
      SyntaxAnalysis.onParsed[Observation[String]](files) { source =>
        source.collect {
          case Defn.Def(_, name, _, paramss, _, _) if {
            val r = for {
              params <- paramss.lastOption
              param <- params.headOption
              } yield param.mods.exists { case mod"implicit" => true; case _ => false }
            r.getOrElse(false)
          } =>
            Observation(name.value,
                        name.pos.start.line + 1,
                        s"implicit param list")
        }
      }
    Observation.markdownTable(results)
  }

  def main(args: Array[String]): Unit = {
    val corpus = Corpus("https://github.com/scalameta/scalafmt/releases/download/v0.7.0-RC1/corpus.zip", _ => true)
    val result = symbolicInfix(Corpus.fastparse)
    Files.write(Paths.get("target", "results.txt"), result.getBytes())
    println(result)
  }
}
