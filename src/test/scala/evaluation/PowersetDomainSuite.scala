package evaluation

import org.scalatest.FunSuite
import xml._
import analysis.domain.powerset._
import xpath.NodeSetValue

class PowersetDomainSuite extends FunSuite {
  type N = PowersetXMLDomain.N
  type L = PowersetXMLDomain.L
  val xmlDom = PowersetXMLDomain.D
  val xpathDom = PowersetXPathXMLDomain

  val root = XMLParser.parseDocument(<root><a/><b/><c/><d/></root>)
  val a = root.elem.children(0).asInstanceOf[XMLElement]
  val b = root.elem.children(1).asInstanceOf[XMLElement]
  val c = root.elem.children(2).asInstanceOf[XMLElement]
  val d = root.elem.children(3).asInstanceOf[XMLElement]

  val root2 = XMLParser.parseDocument(<root><a/><b/><c/><d/></root>)
  val a2 = root2.elem.children(0).asInstanceOf[XMLElement]
  val b2 = root2.elem.children(1).asInstanceOf[XMLElement]
  val c2 = root2.elem.children(2).asInstanceOf[XMLElement]
  val d2 = root2.elem.children(3).asInstanceOf[XMLElement]

  val e1 = XMLParser.parse(<e1/>).asInstanceOf[XMLElement]
  val e2 = XMLParser.parse(<e2/>).asInstanceOf[XMLElement]

  test("Get root") {
    assertResult(Some(Set(root))) { xmlDom.getRoot(Some(Set(a))) }
    assertResult(Some(Set(root))) { xmlDom.getRoot(Some(Set(a, b, c))) }
    assertResult(Some(Set(root, root2))) { xmlDom.getRoot(Some(Set(a, a2))) }
    assertResult(Some(Set(root, root2))) { xmlDom.getRoot(Some(Set(a, b, c, d, a2, b2, c2, d2))) }
  }

  test("Lift to list") {
    val child = XMLParser.parse(<child/>).asInstanceOf[XMLElement]
    assertResult(Some(Set(List(child)))) { xmlDom.liftList(List(xmlDom.lift(child))) }
  }

  test("Append children") {
    val child = xmlDom.liftList(List(xmlDom.lift(XMLParser.parse(<child/>))))
    assertResult(Some(Set(XMLParser.parse(<e1><child/></e1>)))) {
      xmlDom.appendChildren(Some(Set(e1)), child)
    }
    assertResult(Some(Set(XMLParser.parse(<e1><child/></e1>), XMLParser.parse(<e2><child/></e2>)))) {
      xmlDom.appendChildren(Some(Set(e1, e2)), child)
    }
  }

  test("Add attributes") {
    val attr = xmlDom.liftList(List(xmlDom.lift(XMLAttribute("name", "value"))))
    assertResult(Some(Set(XMLParser.parse(<e1 name="value"/>)))) {
      xmlDom.addAttributes(Some(Set(e1)), attr)
    }
    assertResult(Some(Set(XMLParser.parse(<e1 name="value"/>), XMLParser.parse(<e2 name="value"/>)))) {
      xmlDom.addAttributes(Some(Set(e1, e2)), attr)
    }

    val attr2: L = Some(Set(
      List(XMLAttribute("attr1", "1"), XMLAttribute("attr2", "2")), // first alternative
      List(XMLAttribute("attr1", "-1"), XMLAttribute("attr2", "-2")) // second alternative
    ))

    assertResult(Some(Set( // 2 x 2 = 4 possible results
      XMLParser.parse(<e1 attr1="1" attr2="2"/>),
      XMLParser.parse(<e1 attr1="-1" attr2="-2"/>),
      XMLParser.parse(<e2 attr1="1" attr2="2"/>),
      XMLParser.parse(<e2 attr1="-1" attr2="-2"/>)
    ))) {
      xmlDom.addAttributes(Some(Set(e1, e2)), attr2)
    }
  }

  test("List concatenation") {
    val attr1a = XMLAttribute("attr1", "1")
    val attr1b = XMLAttribute("attr1", "-1")
    val attr2 = XMLAttribute("attr2", "2")
    val attr3 = XMLAttribute("attr3", "3")

    val l1a = xmlDom.liftList(List(xmlDom.lift(attr1a)))
    val l2 = xmlDom.liftList(List(xmlDom.lift(attr2)))
    val l3 = xmlDom.liftList(List(xmlDom.lift(attr3)))

    val l1ab: L = Some(Set(List(attr1a), List(attr1b)))
    val l12: L = Some(Set(List(attr1a), List(attr1a, attr2)))

    assertResult(Some(Set(List(attr1a, attr2)))) { xmlDom.listConcat(l1a, l2) }
    assertResult(Some(Set(List(attr1a, attr2, attr3)))) { xmlDom.listConcat(xmlDom.listConcat(l1a, l2), l3) }

    assertResult(Some(Set(List(attr1a, attr2), List(attr1b, attr2)))) { xmlDom.listConcat(l1ab, l2) }
    assertResult(Some(Set(List(attr1a, attr2, attr3), List(attr1b, attr2, attr3)))) { xmlDom.listConcat(xmlDom.listConcat(l1ab, l2), l3) }

    assertResult(Some(Set(List(attr1a, attr2, attr3), List(attr1a, attr3)))) { xmlDom.listConcat(l12, l3) }

    // check associativity of concatenation
    assert(xmlDom.listConcat(xmlDom.listConcat(l1a, l2), l3) == xmlDom.listConcat(l1a, xmlDom.listConcat(l2, l3)))
  }

  test("Lift node set (single)") {
    val n1: N = Some(Set(a))
    val n2: N = Some(Set(b))
    val n3: N = Some(Set(c))

    val input = Set(n1, n2, n3)

    assertResult(Some(Set(NodeSetValue(List(a, b, c))))) {
      xpathDom.liftNodeSet(input)
    }
  }

  test("Lift node set (multiple)") {
    val n1: N = Some(Set(a, b)) // either a or b
    val n2: N = Some(Set(a, b, c)) // either a, b or c
    val n3: N = Some(Set(d)) // exactly d

    val input = Set(n1, n2, n3)
    val expected = Some(Set(
      NodeSetValue(List(b, d)),
      NodeSetValue(List(a, b, d)),
      NodeSetValue(List(b, c, d)),
      NodeSetValue(List(a, d)),
      NodeSetValue(List(a, c, d))
    ))

    assertResult(expected) {
      xpathDom.liftNodeSet(input)
    }
  }
}
