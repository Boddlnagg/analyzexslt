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
    // taken from https://en.wikipedia.org/wiki/XSLT
    val data =
      <persons>
        <person username="JS1">
          <name>John</name>
          <family-name>Smith</family-name>
        </person>
        <person username="MI1">
          <name>Morka</name>
          <family-name>Ismincius</family-name>
        </person>
      </persons>

    XMLElement(data)
  }

  test("W3Schools example") {
    // taken from http://www.w3schools.com/dom/dom_nodes.asp
    val data =
      <bookstore>
        <book category="cooking">
          <title lang="en">Everyday Italian</title>
          <author>Giada De Laurentiis</author>
          <year>2005</year>
          <price>30.00</price>
        </book>
        <book category="children">
          <title lang="en">Harry Potter</title>
          <author>J K. Rowling</author>
          <year>2005</year>
          <price>29.99</price>
        </book>
        <book category="web">
          <title lang="en">XQuery Kick Start</title>
          <author>James McGovern</author>
          <author>Per Bothner</author>
          <author>Kurt Cagle</author>
          <author>James Linn</author>
          <author>Vaidyanathan Nagarajan</author>
          <year>2003</year>
          <price>49.99</price>
        </book>
        <book category="web" cover="paperback">
          <title lang="en">Learning XML</title>
          <author>Erik T. Ray</author>
          <year>2003</year>
          <price>39.95</price>
        </book>
      </bookstore>

    XMLElement(data)
  }
}
