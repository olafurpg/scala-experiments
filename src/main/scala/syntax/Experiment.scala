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
  def runAnalysis[T: ClassTag](
      analysis: PartialFunction[Tree, T]): mutable.Buffer[T] = {
    val results = new CopyOnWriteArrayList[T]
    val counter = new AtomicInteger()
    ScalaFile.getAll.toVector.par.foreach { file =>
      val n = counter.incrementAndGet()
      if (n % 1000 == 0) {
        println(n)
      }
      try {
        file.read.parse[Source] match {
          case Parsed.Success(ast) =>
            ast.collect(analysis).foreach { t =>
              results.add(t)
            }
          case _ =>
        }
      } catch {
        case NonFatal(e) =>
      }
    }
    results.asScala
  }

  type x = {
    val y: Int
  }

  def main(args: Array[String]): Unit = {
    println("Experiment!!!")
    val compounds = runAnalysis[Type.Compound] {
      case t: Type.Compound => t
    }
    val vals = new DescriptiveStatistics()
    val defs = new DescriptiveStatistics()
    val types = new DescriptiveStatistics()
    compounds.foreach { x =>
      x.refinement.foreach {
        case _: Decl.Def => defs.addValue(1.0)
        case _: Decl.Val => vals.addValue(1.0)
        case _: Decl.Type => types.addValue(1.0)
      }
    }
    println(compounds.length)
    println("vals: " + vals.getSum)
    println("defs: " + defs.getSum)
    println("types: " + types.getSum)
  }
}
