import org.scalatest.FunSuite

class ParseDataSuite extends FunSuite {
  test("Minimal example with comments") {
    // taken from https://en.wikipedia.org/wiki/XSLT
    val data =
      <persons>
        <!-- This is a comment -->
      </persons>

    XMLElement(data)
  }

  test("Wikipedia example") {
    XMLElement(TestData.WikipediaData)
  }

  test("W3Schools example") {
    XMLElement(TestData.W3SchoolsData)
  }
}
