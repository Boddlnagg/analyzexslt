import org.scalatest.FunSuite

class MatchPatternSuite extends FunSuite {
  val root = XMLRoot(<a><b x="y"><c/></b><b/></a>)

  val a = root.elem
  val b1 = a.children(0).asInstanceOf[XMLElement]
  val b2 = a.children(1).asInstanceOf[XMLElement]
  val c = b1.children(0)
  val x = b1.attributes(0)

  test("/") {
    val pattern = XPathExpr("/").asInstanceOf[LocationPath]
    assertResult(true) { XPathMatcher.matches(root, pattern) }
    assertResult(false) { XPathMatcher.matches(a, pattern) }
  }

  test("/a") {
    val pattern = XPathExpr("/a").asInstanceOf[LocationPath]
    assertResult(false) { XPathMatcher.matches(root, pattern) }
    assertResult(true) { XPathMatcher.matches(a, pattern) }
    assertResult(false) { XPathMatcher.matches(b1, pattern) }
  }

  test("/a/b") {
    val pattern = XPathExpr("/a/b").asInstanceOf[LocationPath]
    assertResult(false) { XPathMatcher.matches(root, pattern) }
    assertResult(false) { XPathMatcher.matches(a, pattern) }
    assertResult(true) { XPathMatcher.matches(b1, pattern) }
    assertResult(true) { XPathMatcher.matches(b2, pattern) }
  }

  test("//b") {
    val pattern = XPathExpr("//b").asInstanceOf[LocationPath]
    assertResult(false) { XPathMatcher.matches(root, pattern) }
    assertResult(false) { XPathMatcher.matches(a, pattern) }
    assertResult(true) { XPathMatcher.matches(b1, pattern) }
    assertResult(true) { XPathMatcher.matches(b2, pattern) }
  }

  test("a//c") {
    val pattern = XPathExpr("a//c").asInstanceOf[LocationPath]
    assertResult(false) { XPathMatcher.matches(root, pattern) }
    assertResult(false) { XPathMatcher.matches(a, pattern) }
    assertResult(false) { XPathMatcher.matches(b1, pattern) }
    assertResult(true) { XPathMatcher.matches(c, pattern) }
  }

  test("c") {
    val pattern = XPathExpr("c").asInstanceOf[LocationPath]
    assertResult(false) { XPathMatcher.matches(root, pattern) }
    assertResult(false) { XPathMatcher.matches(a, pattern) }
    assertResult(false) { XPathMatcher.matches(b1, pattern) }
    assertResult(true) { XPathMatcher.matches(c, pattern) }
  }

  test("//c") {
    val pattern = XPathExpr("//c").asInstanceOf[LocationPath]
    assertResult(false) { XPathMatcher.matches(root, pattern) }
    assertResult(false) { XPathMatcher.matches(a, pattern) }
    assertResult(false) { XPathMatcher.matches(b1, pattern) }
    assertResult(true) { XPathMatcher.matches(c, pattern) }
  }

  test("@x") {
    val pattern = XPathExpr("@x").asInstanceOf[LocationPath]
    assertResult(false) { XPathMatcher.matches(root, pattern) }
    assertResult(false) { XPathMatcher.matches(a, pattern) }
    assertResult(false) { XPathMatcher.matches(b1, pattern) }
    assertResult(false) { XPathMatcher.matches(c, pattern) }
    assertResult(true) { XPathMatcher.matches(x, pattern) }
  }

  test("b/@x") {
    val pattern = XPathExpr("b/@x").asInstanceOf[LocationPath]
    assertResult(false) { XPathMatcher.matches(root, pattern) }
    assertResult(false) { XPathMatcher.matches(a, pattern) }
    assertResult(false) { XPathMatcher.matches(b1, pattern) }
    assertResult(false) { XPathMatcher.matches(c, pattern) }
    assertResult(true) { XPathMatcher.matches(x, pattern) }
  }

  test("/a/b/@x") {
    val pattern = XPathExpr("/a/b/@x").asInstanceOf[LocationPath]
    assertResult(false) { XPathMatcher.matches(root, pattern) }
    assertResult(false) { XPathMatcher.matches(a, pattern) }
    assertResult(false) { XPathMatcher.matches(b1, pattern) }
    assertResult(false) { XPathMatcher.matches(c, pattern) }
    assertResult(true) { XPathMatcher.matches(x, pattern) }
  }

  test("@x/c") {
    val pattern = XPathExpr("@x/c").asInstanceOf[LocationPath]
    assertResult(false) { XPathMatcher.matches(root, pattern) }
    assertResult(false) { XPathMatcher.matches(a, pattern) }
    assertResult(false) { XPathMatcher.matches(b1, pattern) }
    assertResult(false) { XPathMatcher.matches(c, pattern) }
    assertResult(false) { XPathMatcher.matches(x, pattern) }
  }
}
