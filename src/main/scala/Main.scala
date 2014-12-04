import xpath.XPathParser

object Main {
  def main(args: Array[String]) {
    // TODO: implement command line interface for analyzing XSLT files
    val expr = XPathParser.parse("/a/b/c[@test][1]")
    println(expr)
  }
}