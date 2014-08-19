import org.scalatest.FunSuite

class ParsePatternSuite extends FunSuite {
  test("Accept valid patterns") {
    // these patterns are taken from the spec, section 5.2
    val patterns = List(
      "para",// matches any para element
      "*", // matches any element
      "chapter|appendix", // matches any chapter element and any appendix element
      "olist/item", // matches any item element with an olist parent
      "appendix//para", // matches any para element with an appendix ancestor element
      "/", // matches the root node
      "text()", // matches any text node
      "processing-instruction()", // matches any processing instruction
      "node()", // matches any node other than an attribute node and the root node
      //"id(\"W11\")", // matches the element with unique ID W11 (CURRENTLY NOT SUPPORTED)
      "para[1]", // matches any para element that is the first para child element of its parent
      "*[position()=1 and self::para]", // matches any para element that is the first child element of its parent
      "para[last()=1]", // matches any para element that is the only para child element of its parent
      "items/item[position()>1]", // matches any item element that has a items parent and that is not the first item child of its parent
      "item[position() mod 2 = 1]", // would be true for any item element that is an odd-numbered item child of its parent.
      "div[@class=\"appendix\"]//p", // matches any p element with a div ancestor element that has a class attribute with value appendix
      "@class", // matches any class attribute (not any element that has a class attribute)
      "@*", // matches any attribute
      "appendix//ulist/item[position()=1]" // matches if the node is an item element and the predicate is true and the parent matches appendix//ulist
    )

    patterns.foreach(p => {
      val parsed = XPathExpr.parse(p)
      assert(XPathExpr.isPattern(parsed), f"$p must be recognized as valid pattern")
    })
  }

  test("Reject invalid patterns") {
    val notPatterns = List(
      "*[self::a or self::b][p(.)]/c/descendant-or-self::c", // taken from http://stackoverflow.com/a/9071744
      "boolean('')"
    )

    notPatterns.foreach(p => {
      val parsed = XPathExpr.parse(p)
      assert(!XPathExpr.isPattern(parsed), f"$p must not be recognized as valid pattern")
    })
  }
}
