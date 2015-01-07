import java.io.File

import xslt.{UnsupportedFeatureException, XSLTFeatureAnalyzer}

import scala.collection.mutable.{Map => MutMap, MutableList => MutList, Set => MutSet}
import scala.xml.XML

object Main {
  def main(args: Array[String]) {
    // TODO: implement command line interface for analyzing XSLT files (with parameters/options)
    val map: MutMap[File, Map[String, String]] = MutMap()
    val keys: MutSet[String] = MutSet()
    val files: MutList[File] = MutList()

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
          val xml = XML.loadFile(file)
          try {
            val result = XSLTFeatureAnalyzer.collectFeatures(xml)
            keys ++= result.keys
            map += file -> result
            files += file
          } catch {
            case e: UnsupportedFeatureException => println(f"Could not analyze features in $file: Unsupported feature: ${e.getMessage}")
          }
        case _ => ()
      }
    }

    // Output as list of features
    for (file <- files) {
      println(f"XSLT features used in $file")
      for ((key, value) <- map(file)) {
        println(f"- $key: $value")
      }
    }

    // Output as CSV ('files' contains the columns of the output table; 'map(file)' contains the rows)
    println(f"'',${files.map("'" + _.getParentFile.getName + "'").mkString(",")}") // print header
    for (feature <- keys) {
      println(f"'$feature',${files.map("'" + map(_).getOrElse(feature, "-") + "'").mkString(",")}")
    }
  }
}