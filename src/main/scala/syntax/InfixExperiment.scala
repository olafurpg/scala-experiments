package syntax

import scala.meta.Token
import scala.meta.Tree

case class InfixExperiment(tok: Token, owner: Tree, startOfLine: Boolean) {
  def ownerName: String =
    owner.parent.get.getClass.getName
      .stripPrefix("scala.meta.")
      .replace("$", ".")
      .replaceFirst("\\.[a-zA-Z]*$", "")
}

object InfixExperiment {
  implicit val infixExperimentShow: Show[InfixExperiment] =
    Show.instance[InfixExperiment](
      x => s"${x.tok.syntax}", // : ${x.ownerName}",
      x => Experiment.linenumber(x.owner)
    )
}