package evaluation

import analysis._
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
    assertResult(Set(Root)) { matchTop(pattern) }
  }

  test("*") {
    val pattern = XPathParser.parse("*").asInstanceOf[LocationPath]
    assertResult(Set("*/*", "/*")) { matchTop(pattern).map(_.toString) }
  }

  test("/a") {
    val pattern = XPathParser.parse("/a").asInstanceOf[LocationPath]
    assertResult(Set("/a")) { matchTop(pattern).map(_.toString) }
  }

  test("/a/b/c") {
    val pattern = XPathParser.parse("/a/b/c").asInstanceOf[LocationPath]
    assertResult(Set("/a/b/c")) { matchTop(pattern).map(_.toString) }
  }

  test("a") {
    val pattern = XPathParser.parse("a").asInstanceOf[LocationPath]
    assertResult(Set("*/a", "/a")) { matchTop(pattern).map(_.toString) }
  }

  test("a/b/c") {
    val pattern = XPathParser.parse("a/b/c").asInstanceOf[LocationPath]
    assertResult(Set("*/a/b/c", "/a/b/c")) { matchTop(pattern).map(_.toString) }
  }

  test("a/@b/c") {
    // this path never matches, because attributes can't have children.
    val pattern = XPathParser.parse("a/@b/c").asInstanceOf[LocationPath]
    assertResult(Set()) { matchTop(pattern).map(_.toString) }
  }

  test("a/b/@c") {
    val pattern = XPathParser.parse("a/b/@c").asInstanceOf[LocationPath]
    assertResult(Set("*/a/b/@c", "/a/b/@c")) { matchTop(pattern).map(_.toString) }
  }

  test("/@c") {
    // this path never matches, because the root node can not have attributes
    val pattern = XPathParser.parse("/@c").asInstanceOf[LocationPath]
    assertResult(Set()) { matchTop(pattern).map(_.toString) }
  }

  test("a/text()/c") {
    // this path never matches, because text nodes can't have children.
    val pattern = XPathParser.parse("a/text()/c").asInstanceOf[LocationPath]
    assertResult(Set()) { matchTop(pattern).map(_.toString) }
  }

  test("a/b/text()") {
    val pattern = XPathParser.parse("a/b/text()").asInstanceOf[LocationPath]
    assertResult(Set("*/a/b/text()", "/a/b/text()")) { matchTop(pattern).map(_.toString) }
  }

  test("/text()") {
    // this path never matches, because the root node only has an element node child
    val pattern = XPathParser.parse("/text()").asInstanceOf[LocationPath]
    assertResult(Set()) { matchTop(pattern).map(_.toString) }
  }

  test("a/comment()/c") {
    // this path never matches, because comment nodes can't have children.
    val pattern = XPathParser.parse("a/comment()/c").asInstanceOf[LocationPath]
    assertResult(Set()) { matchTop(pattern).map(_.toString) }
  }

  test("a/b/comment()") {
    val pattern = XPathParser.parse("a/b/comment()").asInstanceOf[LocationPath]
    assertResult(Set("*/a/b/comment()", "/a/b/comment()")) { matchTop(pattern).map(_.toString) }
  }

  test("a/b/node()") {
    val pattern = XPathParser.parse("a/b/node()").asInstanceOf[LocationPath]
    assertResult(Set("*/a/b/text()", "/a/b/text()", "*/a/b/comment()", "/a/b/comment()", "*/a/b/*", "/a/b/*")) {
      matchTop(pattern).map(_.toString)
    }
  }

  test("*/*/@*") {
    val pattern = XPathParser.parse("*/*/@*").asInstanceOf[LocationPath]
    assertResult(Set("*/*/*/@*", "/*/*/@*")) { matchTop(pattern).map(_.toString) }
  }

  /* TODO: This currently doesn't terminate
  test("a//b") {
    val pattern = XPathParser.parse("a//b").asInstanceOf[LocationPath]
    assertResult(Set("a//b"))) { matchTop(pattern).map(_.toString) }
  }*/

  def parse(str: String*): N = str.map(s => XPathPattern.fromString(s)).toSet

  test("a//b  starting from {/*/a/b, /*/a/*/b, /a/b/b, /b/b}") {
    val start = parse("/*/a/b", "/*/a/*/b", "/a/b/b", "/b/b")
    val pattern = XPathParser.parse("a//b").asInstanceOf[LocationPath]
    assertResult(Set("/*/a/b", "/*/a/*/b", "/a/b/b")) { matchIntersect(pattern, start).map(_.toString) }
  }

  test("/* starting from *") {
    val pattern = XPathParser.parse("/*").asInstanceOf[LocationPath]
    assertResult(Set("/*")) { matchIntersect(pattern, parse("*")).map(_.toString) }
  }

  test("a starting from /*") {
    val pattern = XPathParser.parse("a").asInstanceOf[LocationPath]
    assertResult(Set("/a")) { matchIntersect(pattern, parse("/*")).map(_.toString) }
  }

  test("/* starting from a") {
    val pattern = XPathParser.parse("/*").asInstanceOf[LocationPath]
    assertResult(Set("/a")) { matchIntersect(pattern, parse("a")).map(_.toString) }
  }

  test("*/b starting from a/*") {
    val pattern = XPathParser.parse("*/b").asInstanceOf[LocationPath]
    assertResult(Set("/a/b", "*/a/b")) { matchIntersect(pattern, parse("a/*")).map(_.toString) }
  }

  test("*/b starting from /a/*") {
    val pattern = XPathParser.parse("*/b").asInstanceOf[LocationPath]
    assertResult(Set("/a/b")) { matchIntersect(pattern, parse("/a/*")).map(_.toString) }
  }

  test("b/* starting from /*/a") {
    val pattern = XPathParser.parse("b/*").asInstanceOf[LocationPath]
    assertResult(Set("/b/a")) { matchIntersect(pattern, parse("/*/a")).map(_.toString) }
  }

  test("/*/b starting from a/*") {
    val pattern = XPathParser.parse("/*/b").asInstanceOf[LocationPath]
    assertResult(Set("/a/b")) { matchIntersect(pattern, parse("a/*")).map(_.toString) }
  }

  test("a//b//c starting from {/*/b/*/*/c, /a/b/c}") {
    val start = parse("/*/b/*/*/c", "/a/b/c")
    val pattern = XPathParser.parse("a//b//c").asInstanceOf[LocationPath]
    assertResult(Set("/a/b/*/*/c", "/*/b/a/b/c", "/a/b/c")) { matchIntersect(pattern, start).map(_.toString) }
  }

  test("Compare") {
    val pat1 = parse("*")
    val pat2 = parse("/*")
    val pat3 = parse("a", "b")
    val pat4 = parse("/a", "b")
    val pat5 = parse("/a", "/b")
    val pat6 = parse("*/a")
    val pat7 = parse("/*/a")
    val pat8 = parse("/a/a")
    val pat9 = parse("/b/a")
    val pat89 = parse("/a/a", "/b/a")

    assertResult(Equal) { xmlDom.compare(xmlDom.top, xmlDom.top) }
    assertResult(Greater) { xmlDom.compare(xmlDom.top, pat1) }
    assertResult(Greater) { xmlDom.compare(pat1, pat2) }
    assertResult(Less) { xmlDom.compare(pat2, pat1) }
    assertResult(Equal) { xmlDom.compare(pat4, pat4) }
    assertResult(Incomparable) { xmlDom.compare(pat2, pat4) }
    assertResult(Greater) { xmlDom.compare(pat1, pat3) }
    assertResult(Less) { xmlDom.compare(pat5, pat2) }
    assertResult(Greater) { xmlDom.compare(pat3, pat4) }
    assertResult(Greater) { xmlDom.compare(pat1, pat4) }
    assertResult(Greater) { xmlDom.compare(pat1, pat5) }
    assertResult(Greater) { xmlDom.compare(pat6, pat7) }
    assertResult(Greater) { xmlDom.compare(pat7, pat8) }
    assertResult(Greater) { xmlDom.compare(pat6, pat8) }
    assertResult(Greater) { xmlDom.compare(pat6, pat9) }
    assertResult(Greater) { xmlDom.compare(pat7, pat9) }
    assertResult(Incomparable) { xmlDom.compare(pat8, pat9) }
    assertResult(Less) { xmlDom.compare(pat8, pat89) }
    assertResult(Less) { xmlDom.compare(pat9, pat89) }
    assertResult(Greater) { xmlDom.compare(pat7, pat89) }
    assertResult(Greater) { xmlDom.compare(pat6, pat89) }
    assertResult(Greater) { xmlDom.compare(pat1, pat89) }
    assertResult(Incomparable) { xmlDom.compare(pat2, pat89) }
    assertResult(Greater) { xmlDom.compare(pat3, pat89) } // {"a", "b"} > {"a"} > {"/a/a", "/b/a"}
    assertResult(Greater) { xmlDom.compare(parse("a//b"), parse("a/b")) }
    assertResult(Greater) { xmlDom.compare(parse("a//b"), parse("a/*/b")) }
    assertResult(Greater) { xmlDom.compare(parse("a//b"), parse("a//a/b")) }
    assertResult(Greater) { xmlDom.compare(parse("a//b"), parse("a//c/b")) }
    assertResult(Less) { xmlDom.compare(parse("*/a/*"), parse("*//a//*")) }
    assertResult(Less) { xmlDom.compare(parse("*//a/*//*"), parse("*//a//*")) }
    assertResult(Incomparable) { xmlDom.compare(parse("*//a/*//*"), parse("*//b//*")) }
  }

  test("Join") {
    val pat1 = parse("/*")
    val pat2 = parse("*")
    val pat3 = parse("/a")
    val pat4 = parse("/b")
    val pat5 = parse("a", "b")
    val pat6 = parse("b", "c")
    val pat7 = parse("/*/a")
    val pat8 = parse("/a/*")
    val pat9 = parse("*/*")
    val pat37 = xmlDom.join(pat3, pat7) // {"/a", "/*/a"}
    val pat38 = xmlDom.join(pat3, pat8) // {"/a", "/a/*"}

    assertResult(Set("/a", "/*/a")) { pat37.map(_.toString) }

    assertResult(xmlDom.top) {xmlDom.join(pat1, xmlDom.top)}
    assertResult(pat1) {xmlDom.join(pat1, xmlDom.bottom)}

    assertResult(pat1) { xmlDom.join(List(pat1, pat3, pat4)) }
    assertResult(pat2) { xmlDom.join(pat1, pat2) }
    assertResult(Set("/*", "a", "b")) { xmlDom.join(pat1, pat5).map(_.toString) }
    assertResult(pat2) { xmlDom.join(pat2, pat5) }
    assertResult(Set("a", "b", "c")) { xmlDom.join(pat5, pat6).map(_.toString) }
    assertResult(pat2) { xmlDom.join(List(pat5, pat6, pat2)) }
    assertResult(Set("/*", "/*/a")) { xmlDom.join(pat37, pat1).map(_.toString) }
    assertResult(pat2) { xmlDom.join(pat37, pat2) }
    assertResult(Set("/a", "*/*")) { xmlDom.join(pat38, pat9).map(_.toString) }
    assertResult(Set("a", "b")) { xmlDom.join(pat7, pat5).map(_.toString) }
    assertResult(Set("*")) { xmlDom.join(List(xmlDom.bottom, pat2, pat1)).map(_.toString) }
  }

  test("Meet") {
    val pat1 = parse("/*")
    val pat2 = parse("*")
    val pat3 = parse("/a")
    val pat4 = parse("a")
    val pat5 = parse("b")
    val pat45 = parse("a", "b")
    val pat6 = parse("*/a", "*/b")
    val pat7 = parse("a/*")
    val pat8 = parse("*/*")
    val pat67 = xmlDom.join(pat6, pat7) // {"*/a", "*/b", "a/*"}
    val pat9 = parse("/*", "@c")
    val pat10 = parse("@c", "/a")

    assertResult(pat1) {xmlDom.meet(pat1, xmlDom.top)}
    assertResult(xmlDom.bottom) {xmlDom.meet(pat1, xmlDom.bottom)}

    assertResult(Set("/a")) { xmlDom.meet(pat1, pat3).map(_.toString) }
    assertResult(Set("a")) { xmlDom.meet(pat2, pat4).map(_.toString) }
    assertResult(Set("/a")) { xmlDom.meet(pat1, pat4).map(_.toString) }
    assertResult(Set()) { xmlDom.meet(pat4, pat5).map(_.toString) }
    assertResult(Set("a")) { xmlDom.meet(pat45, pat4).map(_.toString) }
    assertResult(Set("a/a", "a/b")) { xmlDom.meet(pat6, pat7).map(_.toString) }
    assertResult(pat67) { xmlDom.meet(pat67, pat8) }
    assertResult(pat45) { xmlDom.meet(pat2, pat45) }
    assertResult(pat7) { xmlDom.meet(pat7, pat2) }
    assertResult(Set("a/a")) { xmlDom.meet(pat7, pat4).map(_.toString) }
    assertResult(Set("@c", "/a")) { xmlDom.meet(pat9, pat10).map(_.toString) }
    assertResult(Set("*/b")) { xmlDom.meet(pat8, pat5).map(_.toString) }
    assertResult(Set("x//a//b//y//*", "a//b//x//y//*", "x//a//y//b//*", "a//x//b//y//*", "a//x//y//b//*", "x//y//a//b//*")) {
      xmlDom.meet(parse("a//b//*"), parse("x//y//*")).map(_.toString) // creates interleavings
    }
    assertResult(Set("a//c//b", "c//a//b", "b//c//b", "c//b//b")) {
      xmlDom.meet(parse("a//b", "b//b"), parse("c//*")).map(_.toString)
    }
  }
}
