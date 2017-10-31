package experiment

import java.nio.file.FileVisitResult
import java.nio.file.Files
import java.nio.file.Path
import java.nio.file.SimpleFileVisitor
import java.nio.file.attribute.BasicFileAttributes
import java.util.concurrent.ConcurrentLinkedQueue
import java.util.concurrent.CopyOnWriteArrayList
import scala.collection.mutable
import scala.collection.parallel.mutable.ParArray
import scala.meta._
import scala.util.control.NonFatal
import org.langmeta.internal.io.PathIO
import org.langmeta.internal.semanticdb.{schema => s}
import org.langmeta.semanticdb.ResolvedName
import org.scalameta.logger

case class SemanticCtx(sdatabase: s.Database) {
  def document = database.documents.head
  def input = document.input
  def tree: Source = input.parse[Source].get
  lazy val database = sdatabase.toDb(None)
  lazy val resolvedSymbols: Map[String, Denotation] = {
    val buf = Map.newBuilder[String, Denotation]
    for {
      document <- sdatabase.documents
      s.ResolvedSymbol(symbol, Some(s.Denotation(flags, name, signature, _))) <- document.symbols
    } {
      buf += (symbol -> Denotation(flags, name, signature, Nil))
    }
    buf.result()
  }
  lazy val resolvedNames: Map[Position, ResolvedName] = {
    val buf = Map.newBuilder[Position, ResolvedName]
    def visit(names: Seq[ResolvedName]) = names.foreach { name =>
      buf += (name.position -> name)
    }
    visit(database.names)
    database.synthetics.foreach(s => visit(s.names))
    database.symbols.foreach(s => visit(s.denotation.names))
    buf.result()
  }
  def denotation(symbol: String): Option[Denotation] =
    resolvedSymbols.get(symbol)
  def denotation(symbol: Symbol): Option[Denotation] =
    denotation(symbol.syntax)
  def symbol(pos: Position): Option[Symbol] =
    resolvedNames.get(pos).map(_.symbol)
}

object SemanticAnalysis {
  var parallel = false
  val root =
    AbsolutePath(BuildInfo.cwd).resolve("target").resolve("v12")

  def run[T](f: SemanticCtx => T): Iterable[(Path, T)] = {
    import scala.collection.JavaConverters._
    val paths = ParArray.newBuilder[Path]
    Files.walkFileTree(
      root.toNIO,
      new SimpleFileVisitor[Path] {
        override def visitFile(
            file: Path,
            attrs: BasicFileAttributes): FileVisitResult = {
          if (PathIO.extension(file) == "semanticdb") {
            paths += file
          }
          FileVisitResult.CONTINUE
        }
      }
    )
    val results = new ConcurrentLinkedQueue[(Path, T)]
    def visit(path: Path): Unit =
      try {
        val sdb = s.Database.parseFrom(Files.readAllBytes(path))
        val ctx = SemanticCtx(sdb)
        results.add(path -> f(ctx))
        print(".")
      } catch {
        case NonFatal(e) =>
          val st = e.getStackTrace
          e.setStackTrace(st.take(20))
          e.printStackTrace()
      }
    paths.result().foreach(visit)
    results.asScala
  }
}
