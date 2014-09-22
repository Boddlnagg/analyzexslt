package evaluation

import org.scalatest.FunSuite
import xml._
import analysis.domain.powerset._
import xpath.NodeSetValue

class PowersetDomainSuite extends FunSuite {
  type N = PowersetXMLDomain.N

  val root = XMLParser.parseDocument(<root><a/><b/><c/><d/></root>)
  val a = root.elem.children(0).asInstanceOf[XMLElement]
  val b = root.elem.children(1).asInstanceOf[XMLElement]
  val c = root.elem.children(2).asInstanceOf[XMLElement]
  val d = root.elem.children(3).asInstanceOf[XMLElement]

  test("Lift node set (single)") {
    val n1: N = Some(Set(a))
    val n2: N = Some(Set(b))
    val n3: N = Some(Set(c))

    val input = Set(n1, n2, n3)

    assertResult(Some(Set(NodeSetValue(List(a, b, c))))) {
      PowersetXPathXMLDomain.liftNodeSet(input)
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
      PowersetXPathXMLDomain.liftNodeSet(input)
    }
  }
}
