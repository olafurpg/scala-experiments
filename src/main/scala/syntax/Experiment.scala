package syntax

import scala.annotation.tailrec
import scala.collection.JavaConverters._
import scala.collection.mutable
import scala.meta.Source
import scala.meta.Tree
import scala.meta.Type
import scala.meta.Type.Compound
import scala.meta._
import scala.reflect.ClassTag
import scala.util.control.NonFatal

import java.util.concurrent.CopyOnWriteArrayList
import java.util.concurrent.atomic.AtomicInteger

object TypeCompoundExperiment {}
object Experiment {

  def collectAnalysis[T](analysis: PartialFunction[Tree, T]): Tree => Seq[T] =
    _.collect(analysis)

  def runAnalysis[T: ClassTag](size: Int)(
      analysis: Tree => Seq[T]): mutable.Buffer[(ScalaFile, T)] = {
    val results = new CopyOnWriteArrayList[(ScalaFile, T)]
    val counter = new AtomicInteger()
    val errors = new AtomicInteger()
    ScalaFile.getAll.take(size).toVector.par.foreach { file =>
      val n = counter.incrementAndGet()
      if (n % 1000 == 0) {
        println(n)
      }
      try {
        file.jFile.parse[Source] match {
          case Parsed.Success(ast) =>
            analysis(ast).foreach { t =>
              results.add(file -> t)
            }
          case _ =>
        }
      } catch {
        case e: org.scalameta.invariants.InvariantFailedException => // scala.meta error
        case e: java.nio.charset.MalformedInputException => // scala.meta error
        case e: java.util.NoSuchElementException => // scala.meta error
        case NonFatal(e) =>
          e.printStackTrace()
          println(e.getClass.getSimpleName)
          val i = errors.incrementAndGet()
          if (i > 100) {
            ???
          }
      }
    }
    results.asScala
  }

  def projectAnalysis: Tree => Seq[Type.Project] = { ast =>
    val builder = Seq.newBuilder[Type.Project]
    def loop(tree: Tree)(gamma: Set[String]): Unit = {
      tree match {
        case t: Type.Project if gamma.contains(t.qual.syntax) =>
          builder += t
        case _ =>
      }
      val newBindings: Seq[String] = tree match {
        case HasTypeParams(tparams) => tparams
        case HasTypeStats(tparams) => tparams
        case _ => Nil
      }
      val newGamma = gamma ++ newBindings
      tree.children.foreach(loop(_)(newGamma))
    }
    loop(ast)(Set.empty[String])
    builder.result()
  }

  def linenumber(t: Tree): Int =
    t.tokens.headOption.map(_.pos.start.line + 1).getOrElse(0)

  def prettyPrint[T](buf: Traversable[(ScalaFile, T)])(
      implicit ev: Show[T]): String = {
    buf.map((ev.pretty _).tupled).mkString("\n")
  }

  def runTypeCompounds(): Unit = {
    val compounds = runAnalysis(30000)(collectAnalysis {
      case t: Type.Compound
          if t.refinement.exists(x => x.is[Decl.Var] || x.is[Defn.Var]) =>
        t
    })
    println("Total: " + compounds.length)
    println(prettyPrint(compounds))
  }

  def runTypeProjections(): Unit = {
    val typProjections = runAnalysis(100000)(projectAnalysis)
    println("Total: " + typProjections.length)
    println(prettyPrint(typProjections))
  }

  @tailrec
  final def parents(tree: Tree,
                    accum: Seq[Tree] = Seq.empty[Tree]): Seq[Tree] = {
    tree.parent match {
      case Some(parent) => parents(parent, parent +: accum)
      case _ => accum
    }
  }

  def runInfixNewline(): Unit = {

    def isSymbolic(nme: String) =
      !nme.exists(x => Character.isLetterOrDigit(x))

    val infixOwners = runAnalysis[InfixExperiment](10000) { t =>
      val owners = TreeOps.getOwners(t)
      val statementsStarts = TreeOps.getStatementStarts(t).keys.toSet

      def isCandidate(ft: FormatToken, tok: Token): Boolean =
        ft.newlines > 0 &&
          isSymbolic(tok.syntax) &&
          !owners(tok).parent.exists(_.is[Pat])

      val fts = FormatToken.formatTokens(t.tokens)
      val next = fts.zip(fts.tail).toMap
      val result = fts.collect {
        // lines ending in infix operator
        case ft @ FormatToken(tok: Token.Ident, _, _)
            if isCandidate(ft, tok) =>
          InfixExperiment(tok, owners(tok), startOfLine = false)
        case ft @ FormatToken(_, tok: Token.Ident, _)
            if isCandidate(ft, tok) &&
              !next.get(ft).exists(_.between.isEmpty) =>
          InfixExperiment(tok, owners(tok), startOfLine = true)
      }
      result.filter(x => statementsStarts(x.tok))
    }
    val (start, end) = infixOwners.partition(_._2.startOfLine)
    start
      .filterNot(_._2.tok.syntax == "@")
      .groupBy(_._2.ownerName)
      .mapValues(x => x -> x.length)
      .toSeq
      .sortBy(_._2._1.length)
      .foreach {
        case (a, (c, b)) =>
          println(s"$a: $b")
          c.foreach {
            case (x, y) =>
              println(InfixExperiment.infixExperimentShow.pretty(x, y))
          }
      }
    println("SOL: " + start.length)
    println("SOL infix: " + start.count(_._2.ownerName.contains("ApplyInfix")))
    println("EOL infix: " + end.count(_._2.ownerName.contains("ApplyInfix")))
    println("Total: " + infixOwners.length)
  }

  def main(args: Array[String]): Unit = {
    println("Experiment!!!")
    runInfixNewline()
//    runTypeProjections()
//    runTypeCompounds()
  }

}
