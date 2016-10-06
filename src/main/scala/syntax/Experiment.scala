package syntax

import scala.collection.mutable
import scala.meta.Source
import scala.meta.Tree
import scala.meta.Type
import scala.meta._
import scala.reflect.ClassTag
import scala.util.control.NonFatal
import scala.collection.JavaConverters._

import java.util.concurrent.CopyOnWriteArrayList
import java.util.concurrent.atomic.AtomicInteger

import org.apache.commons.math3.stat.descriptive.DescriptiveStatistics

object Experiment {

  def collectAnalysis[T](analysis: PartialFunction[Tree, T]): Tree => Seq[T] = {
    ast =>
      ast.collect(analysis)
  }

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

  def runTypeCompounds(): Unit = {
    val compounds = runAnalysis(30000)(collectAnalysis {
      case t: Type.Compound =>
        t.refinement
    })
    compounds.groupBy(_.getClass.getSimpleName).mapValues(_.length).foreach {
      case (k, v) =>
        println(s"$k: $v")
    }
  }

  def runTypeProjections(): Unit = {
    val typProjections = runAnalysis(100000)(projectAnalysis)
    typProjections.foreach {
      case (file, t) =>
        val url = file.githubUrlAtLine(
          t.tokens.headOption.map(_.pos.start.line).getOrElse(0))
        println(s"$url ${t.syntax}")
    }
    println("typProjectsions: " + typProjections.length)
  }

  def main(args: Array[String]): Unit = {
    println("Experiment!!!")
    runTypeProjections()
  }
}

object HasTypeStats {
  def unapply(arg: Tree): Option[Seq[String]] = {
    val stats: Option[Seq[Stat]] = arg match {
      case t: Template if t.stats.nonEmpty => Some(t.stats.get)
      case t: Term.Block => Some(t.stats)
      case _ => None
    }
    stats.map(_.collect {
      case t: Decl.Type => t.name.syntax
      case t: Defn.Type => t.name.syntax
    })
  }

}
object HasTypeParams {
  def unapply(arg: Tree): Option[Seq[String]] = {
    val res = arg match {
      case t: Defn.Def => Some(t.tparams)
      case t: Defn.Trait => Some(t.tparams)
      case t: Defn.Class => Some(t.tparams)
      case _ => None
    }
    res.map(_.map(_.name.syntax))
  }

}
