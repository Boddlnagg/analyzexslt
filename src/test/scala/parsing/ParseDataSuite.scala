package parsing

import data.TestData
import org.scalatest.FunSuite
import xml._

import scala.collection.immutable.TreeSet

class ParseDataSuite extends FunSuite {
  test("Minimal example with comments") {
    // taken from https://en.wikipedia.org/wiki/XSLT
    val data =
      <persons>
        <!-- This is a comment -->
      </persons>

    XMLParser.parseDocument(data)
  }

  test("Wikipedia example") {
    XMLParser.parseDocument(TestData.WikipediaData)
  }

  test("W3Schools example") {
    XMLParser.parseDocument(TestData.W3SchoolsData)
  }

  test("Node ordering") {
    val xml = XMLParser.parseDocument(<root><a><b></b></a><a><b></b></a><c attr="x"></c></root>)
    val root = xml.inner
    val a1 = root.children(0).asInstanceOf[XMLElement]
    val a2 = root.children(1).asInstanceOf[XMLElement]
    val b1 = a1.children(0).asInstanceOf[XMLElement]
    val b2 = a2.children(0).asInstanceOf[XMLElement]
    val c = root.children(2).asInstanceOf[XMLElement]
    val attr = c.attributes(0)

    assertResult(true)(a1 < a2)
    assertResult(true)(a1 <= a2)
    assertResult(false)(a1 > a2)
    assertResult(true)(b1 < b2)
    assertResult(true)(b1 <= b2)
    assertResult(false)(b1 > b2)
    assertResult(false)(b2 <= b1)

    assert(attr > c)
    assert(c > b2)
    assert(b2 > a2)
    assert(a2 > b1)
    assert(b1 > a1)
    assert(a1 > root)
    assert(root > xml)

    assertResult(xml.nodesInOrder) {TreeSet[XMLNode](a2, attr, b1, c, xml, b2, a1, root).toList}
  }

  test("Equality and hash codes") {
    val n1 = XMLParser.parseDocument(TestData.WikipediaData)
    val n2 = XMLParser.parseDocument(TestData.WikipediaData)
    val n3 = XMLParser.parseDocument(<a><b attr="foo"/>bar</a>)
    assert(n1 == n2)
    assert(!(n1 eq n2)) // no pointer equality
    assert(n1 != n3)
    assert(n1.hashCode == n2.hashCode)
    assert(n1.hashCode != n3.hashCode)
    assert(n1.inner.hashCode == n2.inner.hashCode)
    assert(n1.inner.hashCode != n1.hashCode)
  }
}
