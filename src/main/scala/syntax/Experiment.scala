package syntax

import java.nio.file.Files
import java.nio.file.Paths
import scala.meta._
import testkit._

object Experiment {

  def symbolicInfix(corpus: Corpus): String = {
    val files = Corpus.files(corpus).toBuffer.par
    val results = SyntaxAnalysis.onParsed[Observation[Term.Name]](files) {
      source =>
        source.collect {
//          case Term.ApplyInfix(_, name, _, args) if args.length > 1 =>
//            Observation("infix call site with multiple args",
//                        name.pos.start.line,
//                        name)
          case Defn.Def(_, name, _, Seq(params), _, _)
              if !name.value.forall(_.isLetterOrDigit) &&
                params.length > 1 =>
            Observation("symbolic def with multi args",
                        name.pos.start.line,
                        name)
        }
    }
    Observation.markdownTable(results)
  }

  def main(args: Array[String]): Unit = {
    val result = symbolicInfix(Corpus.fastparse)
    Files.write(Paths.get("target", "results.txt"), result.getBytes())
    println(result)
  }
}
