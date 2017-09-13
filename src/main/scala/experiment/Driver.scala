package experiment

import java.nio.file.Files
import java.nio.file.Paths
import java.text.SimpleDateFormat
import java.util.Date
import scala.meta._

object Driver {

  def printReport(report: String): Unit = {
    if (report.nonEmpty) {
      val format = new SimpleDateFormat("yyyy-MM-dd_HH:mm:ss")
      val timestamp = format.format(new Date())
      Files.write(Paths.get("target", "report.md"), report.getBytes)
      Files.write(Paths.get("target", s"report-$timestamp.md"), report.getBytes)
      println(report)
    } else {
      println("Empty report!")
    }
  }

  def main(args: Array[String]): Unit = {
    val report = DefMacroUsage()
    printReport(report)
  }
}
