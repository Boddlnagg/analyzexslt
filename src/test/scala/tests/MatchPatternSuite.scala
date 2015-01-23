package tests

import org.scalatest.FunSuite
import xml._
import xpath._
import xslt.XSLTPatternMatcher

class MatchPatternSuite extends FunSuite {
  val root = XMLParser.parseDocument(<a><b x="y"><c/></b><b/></a>)

  val a = root.children(0).asInstanceOf[XMLElement]
  val b1 = a.children(0).asInstanceOf[XMLElement]
  val b2 = a.children(1).asInstanceOf[XMLElement]
  val c = b1.children(0)
  val x = b1.attributes(0)

  test("/") {
    val pattern = XPathParser.parse("/").asInstanceOf[LocationPath]
    assertResult(true) { XSLTPatternMatcher.matches(root, pattern) }
    assertResult(false) { XSLTPatternMatcher.matches(a, pattern) }
  }

  test("/a") {
    val pattern = XPathParser.parse("/a").asInstanceOf[LocationPath]
    assertResult(false) { XSLTPatternMatcher.matches(root, pattern) }
    assertResult(true) { XSLTPatternMatcher.matches(a, pattern) }
    assertResult(false) { XSLTPatternMatcher.matches(b1, pattern) }
  }

  test("/a/b") {
    val pattern = XPathParser.parse("/a/b").asInstanceOf[LocationPath]
    assertResult(false) { XSLTPatternMatcher.matches(root, pattern) }
    assertResult(false) { XSLTPatternMatcher.matches(a, pattern) }
    assertResult(true) { XSLTPatternMatcher.matches(b1, pattern) }
    assertResult(true) { XSLTPatternMatcher.matches(b2, pattern) }
  }

  test("//b") {
    val pattern = XPathParser.parse("//b").asInstanceOf[LocationPath]
    assertResult(false) { XSLTPatternMatcher.matches(root, pattern) }
    assertResult(false) { XSLTPatternMatcher.matches(a, pattern) }
    assertResult(true) { XSLTPatternMatcher.matches(b1, pattern) }
    assertResult(true) { XSLTPatternMatcher.matches(b2, pattern) }
  }

  test("a//c") {
    val pattern = XPathParser.parse("a//c").asInstanceOf[LocationPath]
    assertResult(false) { XSLTPatternMatcher.matches(root, pattern) }
    assertResult(false) { XSLTPatternMatcher.matches(a, pattern) }
    assertResult(false) { XSLTPatternMatcher.matches(b1, pattern) }
    assertResult(true) { XSLTPatternMatcher.matches(c, pattern) }
  }

  test("a//b") {
    val pattern = XPathParser.parse("a//b").asInstanceOf[LocationPath]
    assertResult(false) { XSLTPatternMatcher.matches(root, pattern) }
    assertResult(false) { XSLTPatternMatcher.matches(a, pattern) }
    assertResult(false) { XSLTPatternMatcher.matches(x, pattern) }
    assertResult(true) { XSLTPatternMatcher.matches(b1, pattern) }
    assertResult(true) { XSLTPatternMatcher.matches(b2, pattern) }
  }

  test("a//a") {
    val pattern = XPathParser.parse("a//a").asInstanceOf[LocationPath]
    assertResult(false) { XSLTPatternMatcher.matches(root, pattern) }
    assertResult(false) { XSLTPatternMatcher.matches(a, pattern) }
    assertResult(false) { XSLTPatternMatcher.matches(x, pattern) }
    assertResult(false) { XSLTPatternMatcher.matches(b1, pattern) }
    assertResult(false) { XSLTPatternMatcher.matches(b2, pattern) }
  }

  test("c") {
    val pattern = XPathParser.parse("c").asInstanceOf[LocationPath]
    assertResult(false) { XSLTPatternMatcher.matches(root, pattern) }
    assertResult(false) { XSLTPatternMatcher.matches(a, pattern) }
    assertResult(false) { XSLTPatternMatcher.matches(b1, pattern) }
    assertResult(true) { XSLTPatternMatcher.matches(c, pattern) }
  }

  test("//c") {
    val pattern = XPathParser.parse("//c").asInstanceOf[LocationPath]
    assertResult(false) { XSLTPatternMatcher.matches(root, pattern) }
    assertResult(false) { XSLTPatternMatcher.matches(a, pattern) }
    assertResult(false) { XSLTPatternMatcher.matches(b1, pattern) }
    assertResult(true) { XSLTPatternMatcher.matches(c, pattern) }
  }

  test("@x") {
    val pattern = XPathParser.parse("@x").asInstanceOf[LocationPath]
    assertResult(false) { XSLTPatternMatcher.matches(root, pattern) }
    assertResult(false) { XSLTPatternMatcher.matches(a, pattern) }
    assertResult(false) { XSLTPatternMatcher.matches(b1, pattern) }
    assertResult(false) { XSLTPatternMatcher.matches(c, pattern) }
    assertResult(true) { XSLTPatternMatcher.matches(x, pattern) }
  }

  test("b/@x") {
    val pattern = XPathParser.parse("b/@x").asInstanceOf[LocationPath]
    assertResult(false) { XSLTPatternMatcher.matches(root, pattern) }
    assertResult(false) { XSLTPatternMatcher.matches(a, pattern) }
    assertResult(false) { XSLTPatternMatcher.matches(b1, pattern) }
    assertResult(false) { XSLTPatternMatcher.matches(c, pattern) }
    assertResult(true) { XSLTPatternMatcher.matches(x, pattern) }
  }

  test("/a/b/@x") {
    val pattern = XPathParser.parse("/a/b/@x").asInstanceOf[LocationPath]
    assertResult(false) { XSLTPatternMatcher.matches(root, pattern) }
    assertResult(false) { XSLTPatternMatcher.matches(a, pattern) }
    assertResult(false) { XSLTPatternMatcher.matches(b1, pattern) }
    assertResult(false) { XSLTPatternMatcher.matches(c, pattern) }
    assertResult(true) { XSLTPatternMatcher.matches(x, pattern) }
  }

  test("@x/c") {
    val pattern = XPathParser.parse("@x/c").asInstanceOf[LocationPath]
    assertResult(false) { XSLTPatternMatcher.matches(root, pattern) }
    assertResult(false) { XSLTPatternMatcher.matches(a, pattern) }
    assertResult(false) { XSLTPatternMatcher.matches(b1, pattern) }
    assertResult(false) { XSLTPatternMatcher.matches(c, pattern) }
    assertResult(false) { XSLTPatternMatcher.matches(x, pattern) }
  }
}
