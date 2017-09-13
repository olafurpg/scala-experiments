package experiment

import java.nio.file.Files
import java.nio.file.Path
import java.util.concurrent.CopyOnWriteArrayList
import scala.collection.mutable
import scala.meta._
import scala.util.control.NonFatal
import org.langmeta.internal.io.PathIO
import org.langmeta.internal.semanticdb.{schema => s}
import org.langmeta.semanticdb.ResolvedName

case class SemanticCtx(tree: Tree, database: Database) {
  private val _denots: Map[Symbol, Denotation] = {
    val buf = Map.newBuilder[Symbol, Denotation]
    database.symbols.foreach { s =>
      buf += (s.symbol -> s.denotation)
    }
    buf.result()
  }
  private val _symbols: Map[Position, ResolvedName] = {
    val buf = Map.newBuilder[Position, ResolvedName]
    def visit(names: Seq[ResolvedName]) = names.foreach { name =>
      buf += (name.position -> name)
    }
    visit(database.names)
    database.synthetics.foreach(s => visit(s.names))
    database.symbols.foreach(s => visit(s.denotation.names))
    buf.result()
  }
  def denotation(symbol: Symbol): Option[Denotation] = _denots.get(symbol)
  def symbol(pos: Position): Option[Symbol] = _symbols.get(pos).map(_.symbol)
}

object SemanticAnalysis {
  def run[T](root: AbsolutePath)(
      f: SemanticCtx => T): mutable.Buffer[(Path, T)] = {
    val results = new CopyOnWriteArrayList[(Path, T)]
    def visit(path: Path): Unit =
      try {
        val sdb = s.Database.parseFrom(Files.readAllBytes(path))
        val mdb = sdb.toDb(None)
        val tree = mdb.documents.head.input.parse[Source].get
        val ctx = SemanticCtx(tree, mdb)
        results.add(path -> f(ctx))
        print(".")
      } catch {
        case NonFatal(e) =>
          val st = e.getStackTrace
          e.setStackTrace(st.take(10))
          e.printStackTrace()
      }
    import scala.collection.JavaConverters._
    val files = Files
      .walk(root.toNIO)
      .iterator()
      .asScala
      .filter { file =>
        Files.isRegularFile(file) &&
        PathIO.extension(file) == "semanticdb"
      }
      .toVector
      .par
    files.foreach(visit)
    results.asScala
  }

}
