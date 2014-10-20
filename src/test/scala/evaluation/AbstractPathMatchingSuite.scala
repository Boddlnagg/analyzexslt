package evaluation

import analysis.AbstractXPathMatcher
import analysis.domain.path._
import analysis.domain.path.XPathPatternDomain.N
import org.scalatest.FunSuite
import xpath.{XPathParser, LocationPath}

class AbstractPathMatchingSuite extends FunSuite {
  val xmlDom = XPathPatternDomain.D
  val matcher = new AbstractXPathMatcher(xmlDom)

  def matchTop(path: LocationPath): N = matchIntersect(path, xmlDom.top)

  def matchIntersect(path: LocationPath, start: N): N = matcher.matches(start, path)._1

  test("/") {
    val pattern = XPathParser.parse("/").asInstanceOf[LocationPath]
    assertResult(Some(Set(Root))) { matchTop(pattern) }
  }

  test("/a") {
    val pattern = XPathParser.parse("/a").asInstanceOf[LocationPath]
    assertResult(Some(Set("/a"))) { matchTop(pattern).map(_.map(_.toString)) }
  }

  test("/a/b/c") {
    val pattern = XPathParser.parse("/a/b/c").asInstanceOf[LocationPath]
    assertResult(Some(Set("/a/b/c"))) { matchTop(pattern).map(_.map(_.toString)) }
  }

  test("a") {
    val pattern = XPathParser.parse("a").asInstanceOf[LocationPath]
    assertResult(Some(Set("a"))) { matchTop(pattern).map(_.map(_.toString)) }
  }

  test("a/b/c") {
    val pattern = XPathParser.parse("a/b/c").asInstanceOf[LocationPath]
    assertResult(Some(Set("a/b/c"))) { matchTop(pattern).map(_.map(_.toString)) }
  }

  test("a/@b/c") {
    // this path never matches, because attributes can't have children.
    val pattern = XPathParser.parse("a/@b/c").asInstanceOf[LocationPath]
    assertResult(Some(Set())) { matchTop(pattern).map(_.map(_.toString)) }
  }

  test("a/b/@c") {
    val pattern = XPathParser.parse("a/b/@c").asInstanceOf[LocationPath]
    assertResult(Some(Set("a/b/@c"))) { matchTop(pattern).map(_.map(_.toString)) }
  }

  test("*/*/@*") {
    val pattern = XPathParser.parse("*/*/@*").asInstanceOf[LocationPath]
    assertResult(Some(Set("*/*/@*"))) { matchTop(pattern).map(_.map(_.toString)) }
  }

  /* TODO: This currently doesn't terminate
  test("a//b") {
    val pattern = XPathParser.parse("a//b").asInstanceOf[LocationPath]
    assertResult(Some(Set("a//b"))) { matchTop(pattern).map(_.map(_.toString)) }
  }*/

  val start1 = AnyElement(None) // "*"
  val start2 = AnyElement(Some(Root)) // "/*"
  val start3 = NamedElement(None, "a") // "a"
  val start4 = AnyElement(Some(NamedElement(None, "a"))) // "a/*"

  test("/* starting from *") {
    val pattern = XPathParser.parse("/*").asInstanceOf[LocationPath]
    assertResult(Some(Set("/*"))) { matchIntersect(pattern, Some(Set(start1))).map(_.map(_.toString)) }
  }

  test("a starting from /*") {
    val pattern = XPathParser.parse("a").asInstanceOf[LocationPath]
    assertResult(Some(Set("/a"))) { matchIntersect(pattern, Some(Set(start2))).map(_.map(_.toString)) }
  }

  test("/* starting from a") {
    val pattern = XPathParser.parse("/*").asInstanceOf[LocationPath]
    assertResult(Some(Set("/a"))) { matchIntersect(pattern, Some(Set(start3))).map(_.map(_.toString)) }
  }

  test("*/b starting from a/*") {
    val pattern = XPathParser.parse("*/b").asInstanceOf[LocationPath]
    assertResult(Some(Set("a/b"))) { matchIntersect(pattern, Some(Set(start4))).map(_.map(_.toString)) }
  }

  test("/*/b starting from a/*") {
    val pattern = XPathParser.parse("/*/b").asInstanceOf[LocationPath]
    assertResult(Some(Set("/a/b"))) { matchIntersect(pattern, Some(Set(start4))).map(_.map(_.toString)) }
  }
}
