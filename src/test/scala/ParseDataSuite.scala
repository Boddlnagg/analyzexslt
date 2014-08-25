import org.scalatest.FunSuite
import scala.collection.immutable.TreeSet

class ParseDataSuite extends FunSuite {
  test("Minimal example with comments") {
    // taken from https://en.wikipedia.org/wiki/XSLT
    val data =
      <persons>
        <!-- This is a comment -->
      </persons>

    XMLRoot(data)
  }

  test("Wikipedia example") {
    XMLRoot(TestData.WikipediaData)
  }

  test("W3Schools example") {
    XMLRoot(TestData.W3SchoolsData)
  }

  test("Node ordering") {
    val xml = XMLRoot(<root><a><b></b></a><a><b></b></a><c attr="x"></c></root>)
    val root = xml.elem
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
}
