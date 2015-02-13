import java.io.{IOException, File}

import analysis.XSLTAnalyzer
import analysis.domain.zipper.ZipperDomain
import xslt.{XSLTParser, UnsupportedFeatureException, XSLTFeatureAnalyzer}

import scala.collection.mutable.{Map => MutMap, MutableList => MutList, Set => MutSet}
import scala.xml.XML

case class Config(files: Seq[File] = Seq(), limitRecursion: Option[Int] = None,
                  disableBuiltinTemplates: Boolean = false, prettyPrint: Boolean = false,
                  analyzeFeatures: Boolean = false, featuresCSVOutput: Boolean = false,
                  timeAnalysis: Boolean = false)

object Main {
  def main(args: Array[String]) {
    val parser = new scopt.OptionParser[Config]("xsltanalyze") {
      opt[Int]('r', "limit-recursion") optional() action { (x, c) =>
        c.copy(limitRecursion = Some(x)) } validate { x =>
          if (x >= 0) success else failure("Recursion limit must not be negative.")
        } text "limit depth of recursive template application"
      opt[Unit]('b', "no-builtin") action { (_, c) =>
        c.copy(disableBuiltinTemplates = true) } text "disable built-in template rules"
      opt[Unit]('p', "pretty") action { (_, c) =>
        c.copy(prettyPrint = true) } text "pretty-print output of analysis in XML-like syntax"
      opt[Unit]('t', "time") action { (_, c) =>
        c.copy(timeAnalysis = true) } text "measure time used for analysis (without parsing)"
      opt[Unit]('f', "features") action { (_, c) =>
        c.copy(analyzeFeatures = true) } text "analyze features instead of running abstract interpreter"
      opt[Unit]('c',"csv") action { (_, c) =>
        c.copy(featuresCSVOutput = true) } text "print result of feature analysis as CSV table (only with -f)"
      help("help") text "prints this usage text"
      arg[File]("<file>...") unbounded() required() action { (x, c) =>
        c.copy(files = c.files :+ x) } text "XSLT stylesheets to analyze"
      checkConfig { c =>
        if (c.featuresCSVOutput && ! c.analyzeFeatures)
          failure("CSV output is only allowed when feature analysis is enabled")
        else if (c.analyzeFeatures && (c.limitRecursion != None || c.disableBuiltinTemplates || c.prettyPrint || c.timeAnalysis))
          failure("Options -r, -b, -p and -t are not allowed when feature analysis is enabled")
        else success
      }
    }

    parser.parse(args, Config()) match {
      case Some(config) => run(config)
      case None => ()
    }
  }

  private def run(config: Config) = {
    val map: MutMap[File, Map[String, String]] = MutMap()
    val keys: MutSet[String] = MutSet()
    val files: MutList[File] = MutList()

    for (file <- config.files) {
      try {
        val xml = XML.loadFile(file)
        if (config.analyzeFeatures) {
          try {
            val result = XSLTFeatureAnalyzer.collectFeatures(xml)
            keys ++= result.keys
            map += file -> result
            files += file
          } catch {
            case e: UnsupportedFeatureException => System.err.println(f"Could not analyze features in $file: Unsupported feature: ${e.getMessage}")
          }
        } else {
          if (config.files.length > 1) println(f"Running abstract interpreter for $file:")
          val stylesheet = XSLTParser.parseStylesheet(xml, config.disableBuiltinTemplates)
          val analyzer = new XSLTAnalyzer(ZipperDomain)
          try {
            val timeStart = System.currentTimeMillis()
            val (subtree, _) = analyzer.transform(stylesheet, ZipperDomain.xmlDom.top, config.limitRecursion)
            val timeEnd = System.currentTimeMillis()
            if (config.prettyPrint) println(subtree.prettyPrint)
            else println(subtree)
            if (config.timeAnalysis) {
              val timeSec = (timeEnd - timeStart: Double) / 1000
              println(f"Analysis completed in ${timeSec}s.")
            }
          } catch {
            case e: StackOverflowError => System.err.println("Running analysis overflowed the Scala stack. Try --limit-recursion.")
          }
        }
      } catch {
        case e: IOException => System.err.println(f"Could not analyze $file: ${e.getMessage}")
      }
    }

    if (config.analyzeFeatures) {
      if (!config.featuresCSVOutput) {
        // Output as list of features
        for (file <- files) {
          println(f"XSLT features used in $file")
          for ((key, value) <- map(file)) {
            println(f"- $key: $value")
          }
        }
      } else {
        // Output as CSV ('files' contains the columns of the output table; 'map(file)' contains the rows)
        println(f"'',${files.map("'" + _.getParentFile.getName + "'").mkString(",")}") // print header
        for (feature <- keys) {
          println(f"'$feature',${files.map(f => "'" + map(f).getOrElse(feature, "-") + "'").mkString(",")}")
        }
      }
    }
  }

}