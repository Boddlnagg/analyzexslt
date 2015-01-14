package tests

import analysis._
import analysis.domain.zipper.Path.PathSetLattice
import analysis.domain.zipper.Subtree.SubtreeLattice
import analysis.domain.zipper.ZipperXMLDomain.N
import analysis.domain.zipper._
import org.scalatest.FunSuite
import xpath.{XPathParser, LocationPath}

class AbstractPathMatchingSuite extends FunSuite {
  val xmlDom = ZipperDomain.xmlDom
  val matcher = new AbstractPatternMatcher(xmlDom)
  type P = Set[Path]
  val latP = PathSetLattice
  val latS = SubtreeLattice
  val TOP = latP.top

  def matches(path: LocationPath, start: P): P = {
    val startNode: N = (latS.top, start)
    val matches = matcher.matches(startNode, path)._1
    matches._2 // extract path result
  }

  def parse(str: String*): P = str.map(s => Path.fromString(s)).toSet

  test("/") {
    val pattern = XPathParser.parse("/").asInstanceOf[LocationPath]
    assertResult(Set(RootPath)) { matches(pattern, TOP) }
  }

  test("*") {
    val pattern = XPathParser.parse("*").asInstanceOf[LocationPath]
    assertResult(Set("*/*", "/*")) { matches(pattern, TOP).map(_.toString) }
  }

  test("/a") {
    val pattern = XPathParser.parse("/a").asInstanceOf[LocationPath]
    assertResult(Set("/a")) { matches(pattern, TOP).map(_.toString) }
  }

  test("/a/b/c") {
    val pattern = XPathParser.parse("/a/b/c").asInstanceOf[LocationPath]
    assertResult(Set("/a/b/c")) { matches(pattern, TOP).map(_.toString) }
  }

  test("a") {
    val pattern = XPathParser.parse("a").asInstanceOf[LocationPath]
    assertResult(Set("*/a", "/a")) { matches(pattern, TOP).map(_.toString) }
  }

  test("a/b/c") {
    val pattern = XPathParser.parse("a/b/c").asInstanceOf[LocationPath]
    assertResult(Set("*/a/b/c", "/a/b/c")) { matches(pattern, TOP).map(_.toString) }
  }

  test("a/@b/c") {
    // this path never matches, because attributes can't have children.
    val pattern = XPathParser.parse("a/@b/c").asInstanceOf[LocationPath]
    assertResult(Set()) { matches(pattern, TOP).map(_.toString) }
  }

  test("a/b/@c") {
    val pattern = XPathParser.parse("a/b/@c").asInstanceOf[LocationPath]
    assertResult(Set("*/a/b/@c", "/a/b/@c")) { matches(pattern, TOP).map(_.toString) }
  }

  test("/@c") {
    // this path never matches, because the root node can not have attributes
    val pattern = XPathParser.parse("/@c").asInstanceOf[LocationPath]
    assertResult(Set()) { matches(pattern, TOP).map(_.toString) }
  }

  test("a/text()/c") {
    // this path never matches, because text nodes can't have children.
    val pattern = XPathParser.parse("a/text()/c").asInstanceOf[LocationPath]
    assertResult(Set()) { matches(pattern, TOP).map(_.toString) }
  }

  test("a/b/text()") {
    val pattern = XPathParser.parse("a/b/text()").asInstanceOf[LocationPath]
    assertResult(Set("*/a/b/text()", "/a/b/text()")) { matches(pattern, TOP).map(_.toString) }
  }

  test("/text()") {
    // this path never matches, because the root node only has an element node child
    val pattern = XPathParser.parse("/text()").asInstanceOf[LocationPath]
    assertResult(Set()) { matches(pattern, TOP).map(_.toString) }
  }

  test("a/comment()/c") {
    // this path never matches, because comment nodes can't have children.
    val pattern = XPathParser.parse("a/comment()/c").asInstanceOf[LocationPath]
    assertResult(Set()) { matches(pattern, TOP).map(_.toString) }
  }

  test("a/b/comment()") {
    val pattern = XPathParser.parse("a/b/comment()").asInstanceOf[LocationPath]
    assertResult(Set("*/a/b/comment()", "/a/b/comment()")) { matches(pattern, TOP).map(_.toString) }
  }

  test("a/b/node()") {
    val pattern = XPathParser.parse("a/b/node()").asInstanceOf[LocationPath]
    assertResult(Set("*/a/b/text()", "/a/b/text()", "*/a/b/comment()", "/a/b/comment()", "*/a/b/*", "/a/b/*")) {
      matches(pattern, TOP).map(_.toString)
    }
  }

  test("*/*/@*") {
    val pattern = XPathParser.parse("*/*/@*").asInstanceOf[LocationPath]
    assertResult(Set("*/*/*/@*", "/*/*/@*")) { matches(pattern, TOP).map(_.toString) }
  }

  // TODO: disabled because of non-termination
  ignore("a//b") {
    val pattern = XPathParser.parse("a//b").asInstanceOf[LocationPath]
    assertResult(Set("a//b")) { matches(pattern, TOP).map(_.toString) }
  }

  // TODO: disabled because of non-termination
  ignore("a//b starting from /a/b//b") {
    val start = parse("/a/b//b")
    val pattern = XPathParser.parse("a//b").asInstanceOf[LocationPath]
    assertResult(Set("a//b")) { matches(pattern, start).map(_.toString) }
  }

  test("/a/*/b starting from //c/b") {
    val start = parse("//c/b")
    val pattern = XPathParser.parse("/a/*/b").asInstanceOf[LocationPath]
    assertResult(Set("/a/c/b")) { matches(pattern, start).map(_.toString) }
  }

  test("a//b  starting from {/*/a/b, /*/a/*/b, /a/b/b, /b/b}") {
    val start = parse("/*/a/b", "/*/a/*/b", "/a/b/b", "/b/b")
    val pattern = XPathParser.parse("a//b").asInstanceOf[LocationPath]
    assertResult(Set("/*/a/b", "/*/a/*/b", "/a/b/b")) { matches(pattern, start).map(_.toString) }
  }

  test("/* starting from *") {
    val pattern = XPathParser.parse("/*").asInstanceOf[LocationPath]
    assertResult(Set("/*")) { matches(pattern, parse("*")).map(_.toString) }
  }

  test("a starting from /*") {
    val pattern = XPathParser.parse("a").asInstanceOf[LocationPath]
    assertResult(Set("/a")) { matches(pattern, parse("/*")).map(_.toString) }
  }

  test("/* starting from a") {
    val pattern = XPathParser.parse("/*").asInstanceOf[LocationPath]
    assertResult(Set("/a")) { matches(pattern, parse("a")).map(_.toString) }
  }

  test("*/b starting from a/*") {
    val pattern = XPathParser.parse("*/b").asInstanceOf[LocationPath]
    assertResult(Set("/a/b", "*/a/b")) { matches(pattern, parse("a/*")).map(_.toString) }
  }

  test("*/b starting from /a/*") {
    val pattern = XPathParser.parse("*/b").asInstanceOf[LocationPath]
    assertResult(Set("/a/b")) { matches(pattern, parse("/a/*")).map(_.toString) }
  }

  test("b/* starting from /*/a") {
    val pattern = XPathParser.parse("b/*").asInstanceOf[LocationPath]
    assertResult(Set("/b/a")) { matches(pattern, parse("/*/a")).map(_.toString) }
  }

  test("/*/b starting from a/*") {
    val pattern = XPathParser.parse("/*/b").asInstanceOf[LocationPath]
    assertResult(Set("/a/b")) { matches(pattern, parse("a/*")).map(_.toString) }
  }

  test("a//b//c starting from {/*/b/*/*/c, /a/b/c}") {
    val start = parse("/*/b/*/*/c", "/a/b/c")
    val pattern = XPathParser.parse("a//b//c").asInstanceOf[LocationPath]
    assertResult(Set("/a/b/*/*/c", "/*/b/a/b/c", "/a/b/c")) { matches(pattern, start).map(_.toString) }
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

    assertResult(Equal) { latP.compare(latP.top, latP.top) }
    assertResult(Greater) { latP.compare(latP.top, pat1) }
    assertResult(Greater) { latP.compare(pat1, pat2) }
    assertResult(Less) { latP.compare(pat2, pat1) }
    assertResult(Equal) { latP.compare(pat4, pat4) }
    assertResult(Incomparable) { latP.compare(pat2, pat4) }
    assertResult(Greater) { latP.compare(pat1, pat3) }
    assertResult(Less) { latP.compare(pat5, pat2) }
    assertResult(Greater) { latP.compare(pat3, pat4) }
    assertResult(Greater) { latP.compare(pat1, pat4) }
    assertResult(Greater) { latP.compare(pat1, pat5) }
    assertResult(Greater) { latP.compare(pat6, pat7) }
    assertResult(Greater) { latP.compare(pat7, pat8) }
    assertResult(Greater) { latP.compare(pat6, pat8) }
    assertResult(Greater) { latP.compare(pat6, pat9) }
    assertResult(Greater) { latP.compare(pat7, pat9) }
    assertResult(Incomparable) { latP.compare(pat8, pat9) }
    assertResult(Less) { latP.compare(pat8, pat89) }
    assertResult(Less) { latP.compare(pat9, pat89) }
    assertResult(Greater) { latP.compare(pat7, pat89) }
    assertResult(Greater) { latP.compare(pat6, pat89) }
    assertResult(Greater) { latP.compare(pat1, pat89) }
    assertResult(Incomparable) { latP.compare(pat2, pat89) }
    assertResult(Greater) { latP.compare(pat3, pat89) } // {"a", "b"} > {"a"} > {"/a/a", "/b/a"}
    assertResult(Greater) { latP.compare(parse("a//b"), parse("a/b")) }
    assertResult(Greater) { latP.compare(parse("a//b"), parse("a/*/b")) }
    assertResult(Greater) { latP.compare(parse("a//b"), parse("a//a/b")) }
    assertResult(Greater) { latP.compare(parse("a//b"), parse("a//c/b")) }
    assertResult(Less) { latP.compare(parse("*/a/*"), parse("*//a//*")) }
    assertResult(Less) { latP.compare(parse("*//a/*//*"), parse("*//a//*")) }
    assertResult(Incomparable) { latP.compare(parse("*//a/*//*"), parse("*//b//*")) }
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
    val pat37 = latP.join(pat3, pat7) // {"/a", "/*/a"}
    val pat38 = latP.join(pat3, pat8) // {"/a", "/a/*"}

    assertResult(Set("/a", "/*/a")) { pat37.map(_.toString) }

    assertResult(latP.top) {latP.join(pat1, latP.top)}
    assertResult(pat1) {latP.join(pat1, latP.bottom)}

    assertResult(pat1) { latP.joinAll(List(pat1, pat3, pat4)) }
    assertResult(pat2) { latP.join(pat1, pat2) }
    assertResult(Set("/*", "a", "b")) { latP.join(pat1, pat5).map(_.toString) }
    assertResult(pat2) { latP.join(pat2, pat5) }
    assertResult(Set("a", "b", "c")) { latP.join(pat5, pat6).map(_.toString) }
    assertResult(pat2) { latP.joinAll(List(pat5, pat6, pat2)) }
    assertResult(Set("/*", "/*/a")) { latP.join(pat37, pat1).map(_.toString) }
    assertResult(pat2) { latP.join(pat37, pat2) }
    assertResult(Set("/a", "*/*")) { latP.join(pat38, pat9).map(_.toString) }
    assertResult(Set("a", "b")) { latP.join(pat7, pat5).map(_.toString) }
    assertResult(Set("*")) { latP.joinAll(List(latP.bottom, pat2, pat1)).map(_.toString) }
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
    val pat67 = latP.join(pat6, pat7) // {"*/a", "*/b", "a/*"}
    val pat9 = parse("/*", "@c")
    val pat10 = parse("@c", "/a")

    assertResult(pat1) {latP.meet(pat1, latP.top)}
    assertResult(latP.bottom) {latP.meet(pat1, latP.bottom)}

    assertResult(Set("/a")) { latP.meet(pat1, pat3).map(_.toString) }
    assertResult(Set("a")) { latP.meet(pat2, pat4).map(_.toString) }
    assertResult(Set("/a")) { latP.meet(pat1, pat4).map(_.toString) }
    assertResult(Set()) { latP.meet(pat4, pat5).map(_.toString) }
    assertResult(Set("a")) { latP.meet(pat45, pat4).map(_.toString) }
    assertResult(Set("a/a", "a/b")) { latP.meet(pat6, pat7).map(_.toString) }
    assertResult(pat67) { latP.meet(pat67, pat8) }
    assertResult(pat45) { latP.meet(pat2, pat45) }
    assertResult(pat7) { latP.meet(pat7, pat2) }
    assertResult(Set("a/a")) { latP.meet(pat7, pat4).map(_.toString) }
    assertResult(Set("@c", "/a")) { latP.meet(pat9, pat10).map(_.toString) }
    assertResult(Set("*/b")) { latP.meet(pat8, pat5).map(_.toString) }
    assertResult(Set("x//a//b//y//*", "a//b//x//y//*", "x//a//y//b//*", "a//x//b//y//*", "a//x//y//b//*", "x//y//a//b//*")) {
      latP.meet(parse("a//b//*"), parse("x//y//*")).map(_.toString) // creates interleavings
    }
    assertResult(Set("a//c//b", "c//a//b", "b//c//b", "c//b//b")) {
      latP.meet(parse("a//b", "b//b"), parse("c//*")).map(_.toString)
    }
  }
}
