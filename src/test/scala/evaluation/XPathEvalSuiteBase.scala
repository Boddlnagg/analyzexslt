package evaluation

import org.scalatest.FunSuite
import xpath._
import xml.{XMLNode, XMLElement, XMLParser}

abstract class XPathEvalSuiteBase extends FunSuite {
  def eval(expr: String, ctxNode: XMLNode = null): XPathValue

  test("Numbers") {
    assertResult(NumberValue(0)) { eval("0") }
    assertResult(NumberValue(1)) { eval("1") }
    assertResult(NumberValue(1.5)) { eval("1.5") }
    assertResult(NumberValue(3)) { eval("1.5 + 1.5") }
    assertResult(NumberValue(0)) { eval("1.5 - 1.5") }
    assertResult(NumberValue(-5)) { eval("-(2 + 3)") }
    assertResult(NumberValue(2)) { eval("4 div 2") }
    assertResult(NumberValue(1)) { eval("4 mod 3") }
  }

  test("Booleans") {
    assertResult(BooleanValue(true)) { eval("true()") }
    assertResult(BooleanValue(false)) { eval("false()") }
    assertResult(BooleanValue(true)) { eval("false() or true()") }
    assertResult(BooleanValue(false)) { eval("true() and false()") }
    assertResult(BooleanValue(false)) { eval("false() or false()") }
    assertResult(BooleanValue(true)) { eval("true() and true()") }
    assertResult(BooleanValue(false)) { eval("not(true())") }
    assertResult(BooleanValue(true)) { eval("not(false())") }
    assertResult(BooleanValue(true)) { eval("true() and not(not(false() or false() or not(false() and true())))") }
  }

  test("Number comparisons") {
    assertResult(BooleanValue(true)) { eval("0 = 0") }
    assertResult(BooleanValue(false)) { eval("0 = 1") }
    assertResult(BooleanValue(true)) { eval("1 > 0") }
    assertResult(BooleanValue(false)) { eval("1 < 0") }
    assertResult(BooleanValue(true)) { eval("1 >= 1") }
    assertResult(BooleanValue(true)) { eval("1 <= 1") }
  }

  test("Location paths") {
    val node = XMLParser.parseDocument(<root><a/><a/><b><c/><a/></b></root>)
    val a1 = node.elem.children(0).asInstanceOf[XMLElement]
    val a2 = node.elem.children(1).asInstanceOf[XMLElement]
    val b = node.elem.children(2).asInstanceOf[XMLElement]
    val c = b.children(0).asInstanceOf[XMLElement]
    val a3 = b.children(1).asInstanceOf[XMLElement]

    assertResult(NodeSetValue(List(node.elem))) { eval("/root", node) }
    assertResult(NodeSetValue(List(a1, a2))) { eval("/root/a", node) }
    assertResult(NodeSetValue(List())) { eval("/a", node) }
    assertResult(NodeSetValue(List(a1, a2, a3))) { eval("//a", node) }
    assertResult(NodeSetValue(List(a1, a2))) { eval("a", node.elem) }
    assertResult(NodeSetValue(List(a1, a2, a3))) { eval("descendant::a", node) }
    assertResult(NodeSetValue(List(b))) { eval("descendant::b", node) }
    assertResult(NodeSetValue(List(b))) { eval("/root/b", node) }
    assertResult(NodeSetValue(List(c))) { eval("descendant::c", node) }
    assertResult(NodeSetValue(List(c))) { eval("/root/b/c", node) }
    assertResult(NodeSetValue(List(c))) { eval("/root/*/c", node) }
    assertResult(NodeSetValue(List(c))) { eval("/root//c", node) }
    assertResult(NodeSetValue(List())) { eval("/*/c", node) }
  }
}