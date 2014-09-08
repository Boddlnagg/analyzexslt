import xpath.XPathParser

object Main {
  def main(args: Array[String]) {
    val expr = XPathParser.parse("/a/b/c[@test][1]")
    println(expr)
  }
}