package experiment

import java.util.concurrent.ConcurrentHashMap
import org.langmeta.semanticdb.ResolvedName

object Acyclic {
  def apply(): String = {
    type Symbol = String
    type Path = String
    val definition = new ConcurrentHashMap[Symbol, Path]()
    val reference = new ConcurrentHashMap[Path, List[Symbol]]
    SemanticAnalysis.run { ctx =>
      ctx.resolvedNames.values.foreach {
        case ResolvedName(pos, sym, defn) =>
          if (defn) {
            definition.put(sym.syntax, pos.input.syntax)
          } else {
            val path = pos.input.syntax
            reference.put(
              path,
              sym.syntax :: reference.getOrDefault(path, Nil)
            )
          }
      }
      Nil
    }
    import scala.collection.JavaConverters._
    definition.asScala.mkString("\n")
  }
}
