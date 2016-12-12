package syntax
import scala.meta._

trait Show[T] {
  def print(e: T): String
  def lineNumber(e: T): Int
  def pretty(file: ScalaFile, t: T): String = {
    val url = file.githubUrlAtLine(this.lineNumber(t))
    s"$url: ${this.print(t)}"
  }
}

object Show {
  implicit def treeShow[T <: Tree]: Show[T] = new Show[T] {
    override def print(e: T): String = e.syntax
    override def lineNumber(e: T): Int = Experiment.linenumber(e)
  }
  def instance[T](f: T => String, g: T => Int): Show[T] = new Show[T] {
    override def print(e: T): String = f(e)
    override def lineNumber(e: T): Int = g(e)
  }
  implicit val compoundShow = new Show[Type.Compound] {
    override def print(e: Type.Compound): String =
      s"""
         |```scala
         |${e.syntax}
         |```""".stripMargin

    override def lineNumber(e: Type.Compound): Int = Experiment.linenumber(e)
  }
}
