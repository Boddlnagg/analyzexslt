import java.io.{FileFilter, File}

import xslt.{UnsupportedFeatureException, XSLTFeatureAnalyzer}

import scala.xml.XML

object Main {
  def main(args: Array[String]) {
    // TODO: implement command line interface for analyzing XSLT files (with parameters/options)
    for (dir <- new File("./xslt-collection").listFiles.toIterator if dir.isDirectory) {
      val xslFiles = dir.listFiles.filter(f => f.getName.toLowerCase.endsWith(".xsl") || f.getName.toLowerCase.endsWith(".xslt"))
      val xslFile = xslFiles.length match {
        case 0 =>
          println(f"Skipping directory $dir (no XSL files found).")
          None
        case 1 =>
          Some(xslFiles(0))
        case _ =>
          val allFile = xslFiles.filter(f => f.getName == "ALL.xsl")
          if (allFile.length != 1) {
            println(f"Multiple XSL files found in directory $dir. Expecting 'ALL.xsl' file.")
            None
          } else {
            Some(allFile(0))
          }
      }

      xslFile match {
        case Some(file) =>
          println(f"Analyzing features in $file")
          val xml = XML.loadFile(file)
          try {
            val result = XSLTFeatureAnalyzer.analyzeFeatures(xml)
            for (feature <- result.toList.sortBy(_.toString)) {
              println(f"- $feature")
            }
          } catch {
            case e: UnsupportedFeatureException => println(f"Could not analyze features in $file: Unsupported feature: ${e.getMessage}")
          }
        case _ => ()
      }
    }
  }
}