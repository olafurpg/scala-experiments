package experiment

import scala.util.Sorting

// Counts most used implicit symbols.
object ImplicitsUsage {
  case class Implicit(string: String)
  def apply(): String = {
    val results = SemanticAnalysis.run { ctx =>
      for {
        d <- ctx.sdatabase.documents
        s <- d.synthetics
        if s.text.indexOf('(') >= 0
        n <- s.names
      } yield n.symbol
    }
    val x = results.flatMap(_._2).groupBy(identity).mapValues(_.size).toArray
    Sorting.quickSort(x)(Ordering.by(-_._2))
    val sb = new java.lang.StringBuilder()
    sb.append("```")
    x.foreach {
      case (symbol, count) =>
        if (count > 10) {
          sb.append(f"$count%10s: ")
            .append(symbol)
            .append('\n')
        }
    }
    sb.append("```")
    sb.toString

  }
}
