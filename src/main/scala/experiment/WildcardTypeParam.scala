package experiment

import scala.meta._
import scala.meta.testkit._

object WildcardTypeParam {
  def apply(): String = {
    val corpus = Corpus.fastparse
    val files = Corpus.files(corpus).toBuffer.par
    val results = SyntaxAnalysis.onParsed(files) { source =>
      var results = List.empty[Observation[Unit]]
      new Traverser {
        override def apply(tree: Tree): Unit = tree match {
          case Type.Param(_, name, _, _, _, _) =>
            name match {
              case Name.Anonymous() =>
                results = Observation("", name.pos.startLine, ()) :: results
              case _ =>
            }
          case _ => super.apply(tree)
        }
      }.apply(source)
      results
    }
    Observation.markdownTable(results.toList)
  }
}
