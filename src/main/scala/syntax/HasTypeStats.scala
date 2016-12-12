package syntax

import scala.meta.Decl
import scala.meta.Defn
import scala.meta.Stat
import scala.meta.Template
import scala.meta.Term
import scala.meta.Tree

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
