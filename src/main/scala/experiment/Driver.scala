package experiment

import java.nio.file.Files
import java.nio.file.Paths
import scala.meta._

object Driver {
  def main(args: Array[String]): Unit = {
    val report = WildcardTypeParam()
    Files.write(Paths.get("target", "report.md"), report.getBytes)
    println(report)
  }
}
