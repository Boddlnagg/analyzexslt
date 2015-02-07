import java.io.File

import analysis.XSLTAnalyzer
import analysis.domain.zipper.ZipperDomain
import xslt.{XSLTParser, UnsupportedFeatureException, XSLTFeatureAnalyzer}

import scala.collection.mutable.{Map => MutMap, MutableList => MutList, Set => MutSet}
import scala.xml.XML

object Main {
  def main(args: Array[String]) {
    // TODO: implement command line interface for analyzing XSLT files (with parameters/options)
    val map: MutMap[File, Map[String, String]] = MutMap()
    val keys: MutSet[String] = MutSet()
    val files: MutList[File] = MutList()

    val analyzeFeatures: Boolean = true
    val inputFileName = "stylesheet.xsl"

    // TODO: use configurable glob file pattern ./xslt-collection/*/stylesheet.xsl
    for (dir <- new File("./xslt-collection").listFiles.toIterator if dir.isDirectory) {
      val xslFiles = dir.listFiles.filter(f => f.getName == inputFileName)
      val xslFile = xslFiles.length match {
        case 0 =>
          println(f"Skipping directory $dir (no XSL files found).")
          None
        case 1 =>
          Some(xslFiles(0))
        case _ =>
          println(f"Skipping directory $dir (multiple XSL files found).")
          None
      }


      xslFile match {
        case Some(file) =>
          val xml = XML.loadFile(file)
          if (analyzeFeatures) {
            try {
              val result = XSLTFeatureAnalyzer.collectFeatures(xml)
              keys ++= result.keys
              map += file -> result
              files += file
            } catch {
              case e: UnsupportedFeatureException => println(f"Could not analyze features in $file: Unsupported feature: ${e.getMessage}")
            }
          } else {
            val stylesheet = XSLTParser.parseStylesheet(xml, disableBuiltinTemplates = false)
            val analyzer = new XSLTAnalyzer(ZipperDomain)
            val (subtree, path) = analyzer.transform(stylesheet, ZipperDomain.xmlDom.top, Some(5))
            println(subtree)
          }
        case _ => ()
      }
    }

    if (analyzeFeatures) {
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
        println(f"'$feature',${files.map(f => "'" + map(f).getOrElse(feature, "-") + "'").mkString(",")}")
      }
    }
  }
}