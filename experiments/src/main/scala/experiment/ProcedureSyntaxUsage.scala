package experiment

import scala.meta._
import scala.meta.testkit.Corpus
import scala.meta.testkit.Observation
import scala.meta.testkit.SyntaxAnalysis

object ProcedureSyntaxUsage {
  def apply(): String = {
    val files = Corpus.files(Corpus.fastparse).toBuffer.par
    val results = SyntaxAnalysis.onParsed(files) { source =>
      source.collect {
        case Defn.Def(_, _, _, _, Some(tpe @ Type.Name("Unit")), _)
            if tpe.tokens.isEmpty =>
          Observation("", tpe.pos.startLine, ())
      }
    }
    Observation.markdownTable(results.toList)
  }
}
