package syntax

import java.nio.file.Files
import java.nio.file.Paths
import scala.meta._
import testkit._

object SyntacticExperiment {

  def symbolicInfix(corpus: Corpus): String = {
    val files = Corpus.files(corpus).toBuffer.par
    val results =
      SyntaxAnalysis.onParsed[Observation[String]](files) { source =>
        source.collect {
          case Term.ApplyInfix(_, name, _, args) if args.length > 1 =>
            Observation(name.value,
                        name.pos.startLine + 1,
                        s"infix call site with ${args.length} args")
          case Defn.Def(_, name, _, Seq(params), _, _)
              if !name.value.headOption.exists(x => x.isLetter || x == '_') &&
                params.length > 1 =>
            Observation(name.value,
                        name.pos.startLine + 1,
                        s"symbolic def with ${params.length} args")
        }
      }
    Observation.markdownTable(results.toList)
  }

  def main(args: Array[String]): Unit = {
    val result = symbolicInfix(Corpus.fastparse)
    Files.write(Paths.get("target", "results.txt"), result.getBytes())
    println(result)
  }
}
