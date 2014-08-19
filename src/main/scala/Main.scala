object Main {

  def main(args: Array[String]) {
    val expr = XPathExpr("/a/b/c[@test][1]")
    println(expr)
  }
}