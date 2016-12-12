package syntax

import scala.meta.Defn
import scala.meta.Tree

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
