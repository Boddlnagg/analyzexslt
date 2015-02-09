package tests

import analysis._
import org.scalatest.FunSuite
import xml._
import analysis.domain.powerset._
import xpath.{XPathParser, NumberValue, NodeSetValue, LocationPath}
import PowersetXMLDomain.N
import PowersetXMLDomain.L
import PowersetXPathDomain.V

import scala.collection.immutable.TreeSet

class PowersetDomainSuite extends FunSuite {
  val xmlDom = PowersetDomain.xmlDom
  val xpathDom = PowersetDomain.xpathDom

  val root = XMLParser.parseDocument(<root><a/><b/><c/><d/></root>)
  val rootelem = root.children(0).asInstanceOf[XMLElement]
  val a = rootelem.children(0).asInstanceOf[XMLElement]
  val b = rootelem.children(1).asInstanceOf[XMLElement]
  val c = rootelem.children(2).asInstanceOf[XMLElement]
  val d = rootelem.children(3).asInstanceOf[XMLElement]

  val root2 = XMLParser.parseDocument(<root><a/><b/><c/><d/></root>)
  val root2elem = root2.children(0).asInstanceOf[XMLElement]
  val a2 = root2elem.children(0).asInstanceOf[XMLElement]
  val b2 = root2elem.children(1).asInstanceOf[XMLElement]
  val c2 = root2elem.children(2).asInstanceOf[XMLElement]
  val d2 = root2elem.children(3).asInstanceOf[XMLElement]

  val e1 = XMLParser.parse(<e1/>).asInstanceOf[XMLElement]
  val e2 = XMLParser.parse(<e2/>).asInstanceOf[XMLElement]

  /** Helper function to create an element with no children or attributes */
  def createElement(name: String): N = xmlDom.createElement(xpathDom.liftString(name), xmlDom.createEmptyList(), xmlDom.createEmptyList())

  test("Get root") {
    assertResult(Some(Set(root))) { xmlDom.getRoot(Some(Set(a))) }
    assertResult(Some(Set(root))) { xmlDom.getRoot(Some(Set(a, b, c))) }
    assertResult(Some(Set(root, root2))) { xmlDom.getRoot(Some(Set(a, a2))) }
    assertResult(Some(Set(root, root2))) { xmlDom.getRoot(Some(Set(a, b, c, d, a2, b2, c2, d2))) }
  }

  test("Lift list") {
    val out1 = XMLParser.parse(<out1/>).asInstanceOf[XMLElement]
    val out2 = XMLParser.parse(<out2/>).asInstanceOf[XMLElement]
    val out3 = XMLParser.parse(<out3/>).asInstanceOf[XMLElement]

    assertResult(Some(Set(List(out1)))) { xmlDom.createSingletonList(createElement("out1")) }
    assertResult(None) { xmlDom.concatLists(xmlDom.createSingletonList(createElement("out1")), xmlDom.createSingletonList(xmlDom.top))} // this is true for this domain
    assertResult(Some(Set(List(out1, out3), List(out2, out3)))) { xmlDom.concatLists(xmlDom.createSingletonList(Some(Set(out1, out2))), xmlDom.createSingletonList(Some(Set(out3)))) }
    // this is true for all domains (if one of the elements is bottom, the resulting list must be bottom)
    assertResult(xmlDom.bottomList) { xmlDom.concatLists(xmlDom.createSingletonList(Some(Set(out1, out2))), xmlDom.createSingletonList(xmlDom.bottom)) }
    // the empty list must be lifted to the empty list
    assertResult(Some(Set(Nil))) { xmlDom.createEmptyList() }
  }

  test("Lift element with children") {
    val child = xmlDom.createSingletonList(createElement("child"))
    assertResult(Some(Set(XMLParser.parse(<e1><child/></e1>)))) {
      xmlDom.createElement(xpathDom.liftString("e1"), xmlDom.createEmptyList(), child)
    }

    val children = xmlDom.joinLists(xmlDom.concatLists(xmlDom.createSingletonList(createElement("child1")), xmlDom.createSingletonList(createElement("child2"))), child)
    assertResult(Some(Set(XMLParser.parse(<e1><child/></e1>), XMLParser.parse(<e1><child1/><child2/></e1>)))) {
      xmlDom.createElement(xpathDom.liftString("e1"), xmlDom.createEmptyList(), children)
    }
  }

  test("Lift element with attributes") {
    val attr = xmlDom.createSingletonList(xmlDom.createAttribute(xpathDom.liftString("name"), xpathDom.liftString("value")))
    assertResult(Some(Set(XMLParser.parse(<e1 name="value"/>)))) {
      xmlDom.createElement(xpathDom.liftString("e1"), attr, xmlDom.createEmptyList())
    }

    val attr2: L = Some(Set(
      List(XMLAttribute("attr1", "1"), XMLAttribute("attr2", "2")), // first alternative
      List(XMLAttribute("attr1", "-1"), XMLAttribute("attr2", "-2")), // second alternative
      Nil // third alternative
    ))

    assertResult(Some(Set(
      XMLParser.parse(<e1 attr1="1" attr2="2"/>),
      XMLParser.parse(<e1 attr1="-1" attr2="-2"/>),
      XMLParser.parse(<e1/>)
    ))) {
      xmlDom.createElement(xpathDom.liftString("e1"), attr2, xmlDom.createEmptyList())
    }
  }

  test("List concatenation") {
    val attr1a = XMLAttribute("attr1", "1")
    val attr1b = XMLAttribute("attr1", "-1")
    val attr2 = XMLAttribute("attr2", "2")
    val attr3 = XMLAttribute("attr3", "3")

    val l1a = xmlDom.createSingletonList(xmlDom.createAttribute(xpathDom.liftString("attr1"), xpathDom.liftString("1")))
    val l2 = xmlDom.createSingletonList(xmlDom.createAttribute(xpathDom.liftString("attr2"), xpathDom.liftString("2")))
    val l3 = xmlDom.createSingletonList(xmlDom.createAttribute(xpathDom.liftString("attr3"), xpathDom.liftString("3")))

    val l1ab: L = Some(Set(List(attr1a), List(attr1b)))
    val l12: L = Some(Set(List(attr1a), List(attr1a, attr2)))

    assertResult(Some(Set(List(attr1a, attr2)))) { xmlDom.concatLists(l1a, l2) }
    assertResult(Some(Set(List(attr1a, attr2, attr3)))) { xmlDom.concatLists(xmlDom.concatLists(l1a, l2), l3) }

    assertResult(Some(Set(List(attr1a, attr2), List(attr1b, attr2)))) { xmlDom.concatLists(l1ab, l2) }
    assertResult(Some(Set(List(attr1a, attr2, attr3), List(attr1b, attr2, attr3)))) { xmlDom.concatLists(xmlDom.concatLists(l1ab, l2), l3) }

    assertResult(Some(Set(List(attr1a, attr2, attr3), List(attr1a, attr3)))) { xmlDom.concatLists(l12, l3) }

    // check associativity of concatenation
    assert(xmlDom.concatLists(xmlDom.concatLists(l1a, l2), l3) == xmlDom.concatLists(l1a, xmlDom.concatLists(l2, l3)))

    // check that empty list concatenation is identity transformation
    assert(xmlDom.concatLists(l12, xmlDom.createEmptyList()) == l12)
  }

  test("Lift node set (single)") {
    val l1: L = xmlDom.createSingletonList(Some(Set(a)))
    val l2: L = xmlDom.createSingletonList(Some(Set(b)))
    val l3: L = xmlDom.createSingletonList(Some(Set(c)))

    val input = xmlDom.concatLists(l1, xmlDom.concatLists(l2, l3))

    assertResult(Some(Set(NodeSetValue(TreeSet(a, b, c))))) {
      xpathDom.createNodeSet(input)
    }
  }

  test("Lift node set (multiple)") {
    val l1: L = xmlDom.createSingletonList(Some(Set(a, b))) // either a or b
    val l2: L = xmlDom.createSingletonList(Some(Set(a, b, c))) // either a, b or c
    val l3: L = xmlDom.createSingletonList(Some(Set(d))) // exactly d

    val input = xmlDom.concatLists(l1, xmlDom.concatLists(l2, l3))
    val expected = Some(Set(
      NodeSetValue(TreeSet(b, d)),
      NodeSetValue(TreeSet(a, b, d)),
      NodeSetValue(TreeSet(b, c, d)),
      NodeSetValue(TreeSet(a, d)),
      NodeSetValue(TreeSet(a, c, d))
    ))

    assertResult(expected) {
      xpathDom.createNodeSet(input)
    }
  }

  test("flatMapWithIndex") {
    val out1 = XMLParser.parse(<out1/>).asInstanceOf[XMLElement]
    val out2 = XMLParser.parse(<out2/>).asInstanceOf[XMLElement]
    val out3 = XMLParser.parse(<out3/>).asInstanceOf[XMLElement]
    val out4 = XMLParser.parse(<out4/>).asInstanceOf[XMLElement]

    val input: L = Some(Set(List(a, b, c), List(c, b), List(a)))
    def transform(node: N, index: V): L = {
      if (node == Some(Set(a))) {
        assert(index == Some(Set(NumberValue(0))))
        Some(Set(List(out1, out2), List(out3)))
      } else if (node == Some(Set(b))) {
        assert(index == Some(Set(NumberValue(1))))
        Some(Set(List(out4)))
      } else if (node == Some(Set(c))) {
        Some(Set(List()))
      } else {
        throw new AssertionError(f"node must be a, b, or c but was $node")
      }
    }

    assertResult(Some(Set(List(out3, out4), List(out1, out2), List(out1, out2, out4), List(out4), List(out3)))) {
      xmlDom.flatMapWithIndex(input, transform)
    }
  }

  test("Match patterns") {
    val doc = XMLParser.parseDocument(<root attr="1" otherattr="foobar"><a/><b><a/><a/></b><b attr="2"/></root>)
    val root = doc.children(0).asInstanceOf[XMLElement]
    val attr1 = root.attributes.filter(_.name == "attr")(0)
    val otherattr = root.attributes.filter(_.name == "otherattr")(0)
    val a1 = root.children(0).asInstanceOf[XMLElement]
    val b1 = root.children(1).asInstanceOf[XMLElement]
    val a2 = b1.children(0).asInstanceOf[XMLElement]
    val a3 = b1.children(1).asInstanceOf[XMLElement]
    val b2 = root.children(2).asInstanceOf[XMLElement]
    val attr2 = b2.attributes(0)

    def pattern(str: String) = XPathParser.parse(str).asInstanceOf[LocationPath]

    val matcher = new AbstractPatternMatcher[N, L, V](PowersetDomain.xmlDom)

    val all: N = Some(Set(doc, root, attr1, otherattr, a1, b1, a2, a3, b2, attr2))

    assertResult((Some(Set(doc)), Some(Set(root, attr1, otherattr, a1, b1, a2, a3, b2, attr2)))) { matcher.matches(all, pattern("/")) }
    assertResult((Some(Set()), all)) { matcher.matches(all, pattern("/a")) }
    assertResult((Some(Set(a1)), Some(Set(doc, root, attr1, otherattr, b1, b2, attr2, a2, a3)))) { matcher.matches(all, pattern("/root/a")) }
    assertResult((Some(Set(a1)), Some(Set(doc, root, attr1, otherattr, b1, b2, attr2, a2, a3)))) { matcher.matches(all, pattern("/*/a")) }
    assertResult((Some(Set(a2, a3)), Some(Set(doc, root, attr1, otherattr, b1, b2, attr2, a1)))) { matcher.matches(all, pattern("/*/*/a")) }
    assertResult((Some(Set()), Some(Set(doc, root, attr1, otherattr, b1, b2, attr2, a1, a2, a3)))) { matcher.matches(all, pattern("/*/*/*/a")) }
    assertResult((Some(Set(a2, a3)), Some(Set(a1)))) { matcher.matches(Some(Set(a1, a2, a3)), pattern("/*/*/a")) }

    assertResult((Some(Set(a1, a2, a3)), Some(Set(doc, root, attr1, otherattr, b1, b2, attr2)))) { matcher.matches(all, pattern("//a")) }
    assertResult((Some(Set(a2, a3)), Some(Set(doc, root, attr1, otherattr, b1, b2, attr2, a1)))) { matcher.matches(all, pattern("b//a")) }
  }

  test("Compare (with booleans)") {
    val stringVal = xpathDom.liftString("String")
    val trueVal = xpathDom.liftBoolean(true)
    val falseVal = xpathDom.liftBoolean(false)
    val anyVal = xpathDom.join(xpathDom.join(trueVal, falseVal), stringVal)

    assertResult(Greater) { xpathDom.compare(trueVal, xpathDom.bottom) }
    assertResult(Greater) { xpathDom.compare(falseVal, xpathDom.bottom) }
    assertResult(Equal) { xpathDom.compare(falseVal, falseVal) }
    assertResult(Incomparable) { xpathDom.compare(trueVal, falseVal) }
    assertResult(Incomparable) { xpathDom.compare(falseVal, trueVal) }
    assertResult(Less) { xpathDom.compare(trueVal, anyVal) }
    assertResult(Less) { xpathDom.compare(falseVal, anyVal) }
    assertResult(Incomparable) { xpathDom.compare(trueVal, stringVal) }
    assertResult(Incomparable) { xpathDom.compare(falseVal, stringVal) }
  }

  test("Transform multiple inputs") {
    val xslt =
      <xsl:stylesheet version='1.0' xmlns:xsl='http://www.w3.org/1999/XSL/Transform'>
        <xsl:template match='/'>
          <reRoot><reNode><xsl:value-of select='/root/node/@val' /> world</reNode></reRoot>
        </xsl:template>
      </xsl:stylesheet>

    val data1 = XMLParser.parseDocument(<root><node val='hello'/></root>)
    val data2 = XMLParser.parseDocument(<root><node val='Hello'/></root>)
    val data3 = XMLParser.parseDocument(<root></root>)

    val data: N = Some(Set(data1, data2, data3))

    val output1 = XMLParser.parseDocument(<reRoot><reNode>hello world</reNode></reRoot>)
    val output2 = XMLParser.parseDocument(<reRoot><reNode>Hello world</reNode></reRoot>)
    val output3 = XMLParser.parseDocument(<reRoot><reNode> world</reNode></reRoot>)

    assertResult(Some(Set(output1, output2, output3))) {
      TransformHelper.transformAbstract(xslt, data, PowersetDomain)
    }
  }
}
