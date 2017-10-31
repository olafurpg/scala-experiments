package experiment

import scala.meta._
import scala.meta.testkit.CorpusFile
import scala.meta.testkit.Observation
import org.scalameta.logger

object UniversalTrait {
  def apply(): String = {
    val x = SemanticAnalysis.run[Seq[Observation[String]]] { ctx =>
      def range(start: Int, end: Int) =
        Position.Range(ctx.input, start, end)
      def point(offset: Int) = range(offset, offset)

      for {
        doc <- ctx.sdatabase.documents
        name <- doc.names
        if name.symbol == "_root_.scala.Any#"
        pos <- name.position.toList
        p = range(pos.start, pos.end)
        ext = point(pos.start - 8).copy(end = pos.start).text
        if ext == "extends " // hack to avoid parsing for faster analysis
      } yield
        Observation(
          p.copy(start = p.start - p.startColumn).text,
          p.startLine,
          ctx.source.url)
    }
    val results = x.flatMap {
      case (cp, obs) =>
        obs.map(o => cp.toCorpusFile -> o)
    }
    Observation.markdownTable(results.toList)
  }
}
