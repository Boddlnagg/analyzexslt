import org.scalatest.FunSuite

class MatchPatternSuite extends FunSuite {
  val xml = XMLRoot(<a><b x="y"><c/></b><b/></a>)

  test("/") {
    assertResult(true) {
      XPathMatcher.matches(xml, XPathExpr("/").asInstanceOf[LocationPath])
    }
    assertResult(false) {
      XPathMatcher.matches(xml.elem, XPathExpr("/").asInstanceOf[LocationPath])
    }
  }

  test("/a") {
    assertResult(false) {
      XPathMatcher.matches(xml, XPathExpr("/a").asInstanceOf[LocationPath])
    }
    assertResult(true) {
      XPathMatcher.matches(xml.elem, XPathExpr("/a").asInstanceOf[LocationPath])
    }
    assertResult(false) {
      XPathMatcher.matches(xml.elem.children(0), XPathExpr("/a").asInstanceOf[LocationPath])
    }
  }

  test("/a/b") {
    assertResult(false) {
      XPathMatcher.matches(xml, XPathExpr("/a/b").asInstanceOf[LocationPath])
    }
    assertResult(false) {
      XPathMatcher.matches(xml.elem, XPathExpr("/a/b").asInstanceOf[LocationPath])
    }
    assertResult(true) {
      XPathMatcher.matches(xml.elem.children(0), XPathExpr("/a/b").asInstanceOf[LocationPath])
    }
  }

  test("//b") {
    assertResult(false) {
      XPathMatcher.matches(xml, XPathExpr("//b").asInstanceOf[LocationPath])
    }
    assertResult(false) {
      XPathMatcher.matches(xml.elem, XPathExpr("//b").asInstanceOf[LocationPath])
    }
    assertResult(true) {
      XPathMatcher.matches(xml.elem.children(0), XPathExpr("//b").asInstanceOf[LocationPath])
    }
  }
}
