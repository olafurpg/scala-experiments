package experiment

import scala.meta._

object Dot {
  def apply(graph: String): String = {
    import java.io.ByteArrayInputStream
    import scala.sys.process._
    val bais = new ByteArrayInputStream(
      s"""digraph X {
         |  rankdir=LR;
         |  bgcolor=transparent;
         |  colorscheme=bugn9;
         |  graph [fontname = "Inconsolata"];
         |  node [shape=box, fontname = "Inconsolata"];
         |  edge [fontname = "Inconsolata"];
         |  $graph
         |}""".stripMargin.toString
        .getBytes("UTF-8")
    )
    val command = List("dot", "-Tsvg")
    val svg = (command #< bais).!!.trim
      .replaceFirst("\n<svg.*?\n", "\n<svg width=\"100%\" height=\"30%\"\n")
    svg
  }
}
