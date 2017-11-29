package experiment

import java.io.FileInputStream
import java.nio.charset.StandardCharsets
import java.nio.file.FileVisitResult
import java.nio.file.Files
import java.nio.file.Path
import java.nio.file.SimpleFileVisitor
import java.nio.file.attribute.BasicFileAttributes
import java.util.concurrent.ConcurrentHashMap
import java.util.concurrent.ConcurrentLinkedQueue
import java.util.concurrent.atomic.AtomicInteger
import scala.collection.parallel.mutable.ParArray
import scala.meta._
import scala.meta.testkit.CorpusFile
import scala.util.control.NonFatal
import experiment.SemanticAnalysis.SourceFile
import org.langmeta.internal.io.PathIO
import org.langmeta.internal.semanticdb.{schema => s}
import org.langmeta.semanticdb.ResolvedName

case class SemanticCtx(sdatabase: s.Database, source: SourceFile) {
  def document = database.documents.head
  lazy val input = {
    val sdoc = sdatabase.documents.head
    Input.VirtualFile(sdoc.filename, sdoc.contents)
  }
  lazy val tree: Source = input.parse[Source].get
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
  case class SourceFile(
      path: Path,
      filename: String,
      url: String,
      commit: String) {
    def toCorpusFile: CorpusFile = CorpusFile("/" + filename, url, commit)
  }

  val root =
    AbsolutePath(BuildInfo.cwd).resolve("target").resolve("v13")

  def run[T](f: SemanticCtx => T): Iterable[(SourceFile, T)] = {
    import scala.collection.JavaConverters._
    val paths = ParArray.newBuilder[SourceFile]
    Files.list(root.toNIO).forEach { project =>
      val metadataPath = project.resolve("metadata.properties")
      if (Files.isRegularFile(metadataPath)) {
        val metadata = {
          val props = new java.util.Properties()
          val in = Files.newInputStream(metadataPath)
          try {
            props.load(in)
          } finally in.close()
          props
        }
        val url = metadata
          .getProperty("url")
          .stripSuffix(".git")
          .replaceFirst("^git@github.com:", "https://github.com/")
        val branch = metadata.getProperty("branch")
        val commit = metadata.getProperty("commit")
        def add(path: Path): Unit = {
          paths += SourceFile(path, "", url, commit)
        }
        Files.walkFileTree(
          project,
          new SimpleFileVisitor[Path] {
            override def visitFile(
                file: Path,
                attrs: BasicFileAttributes): FileVisitResult = {
              if (PathIO.extension(file) == "semanticdb") {
                add(file)
              }
              FileVisitResult.CONTINUE
            }
          }
        )
      }
    }
    val results = new ConcurrentLinkedQueue[(SourceFile, T)]
    val analyzedFilenames = new ConcurrentHashMap[(String, String), Unit]()
    val totalLines = new ConcurrentHashMap[String, AtomicInteger]()
    def visit(file: SourceFile): Unit =
      try {
        val path = file.path
        s.Database.parseFrom(Files.readAllBytes(path)).documents.foreach { d =>
          val sdb = s.Database(d :: Nil)
          analyzedFilenames.computeIfAbsent(
            file.url -> d.filename, { _ =>
              val counter =
                totalLines.computeIfAbsent(file.url, _ => new AtomicInteger())
              counter.addAndGet(d.contents.count(_ == '\n'))
              val ctx = SemanticCtx(sdb, file)
              results.add(
                file.copy(filename = sdb.documents.head.filename) -> f(ctx))
            }
          )
        }
        print(".")
      } catch {
        case NonFatal(e) =>
          val st = e.getStackTrace
          e.setStackTrace(st.take(20))
          e.printStackTrace()
      }
    paths
      .result()
//      .take(1000)
      .foreach(visit)
    println()
    totalLines.asScala.foreach {
      case (url, counter) =>
        println(f"$counter\t$url")
    }
    val totalCount = totalLines.values().asScala.iterator.map(_.get()).sum
    println("TOTAL LINES: " + totalCount)
    results.asScala
  }
}
