object Main {

  def main(args: Array[String]) {
    val expr = XPathExpr.parse("/a/b/c[@test][1]")
    println(expr)
  }
}