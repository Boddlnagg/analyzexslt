import org.scalatest.FunSuite

import xpath._

class ParsePatternSuite extends FunSuite {
  test("Accept valid patterns #1") {
    // these patterns are taken from the spec, section 5.2
    val patterns = List(
      "para",// matches any para element
      "*", // matches any element
      "chapter|appendix", // matches any chapter element and any appendix element
      "olist/item", // matches any item element with an olist parent
      "appendix//para", // matches any para element with an appendix ancestor element
      "/", // matches the root node
      "text()", // matches any text node
      //"processing-instruction()", // matches any processing instruction (NOT IMPLEMENTED)
      "node()", // matches any node other than an attribute node and the root node
      //"id(\"W11\")", // matches the element with unique ID W11 (NOT IMPLEMENTED)
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
      val parsed = XPathParser.parse(p)
      assert(XPathExpr.isPattern(parsed), f"$p must be recognized as valid pattern")
    })
  }

  test("Accept valid patterns #2") {
    // these patterns are taken from http://www.lenzconsulting.com/how-xslt-works/
    val patterns = List(
      "/", // the root node
      "/doc[@format='simple']", // the root element only if its name is doc and it has a format attribute with the value simple
      "bar", // any bar element
      "foo/bar", // any bar element whose parent is a foo element
      //"id('xyz')/foo", // any foo element whose parent is an element that has an ID-typed attribute with the value xyz (NOT IMPLEMENTED)
      "section//para", // any para element that has a section element ancestor
      "@foo", // any attribute named foo
      "@*", // any attribute
      "node()", // any child node (i.e., element, text, comment, or processing instruction)
      "text()", // any text node
      "*", // any element
      //"xyz:*", // any element in the namespace designated by the xyz prefix (NOT IMPLEMENTED)
      //"*[not(self::xyz:*)]", // any element that is not in the namespace designated by the xyz prefix (NOT IMPLEMENTED)
      "para[2]", // any para element that is the second para child of its parent
      "para[last()]" // any para element that is the last para child of its parent
    )

    patterns.foreach(p => {
      val parsed = XPathParser.parse(p)
      assert(XPathExpr.isPattern(parsed), f"$p must be recognized as valid pattern")
    })
  }

  test("Reject invalid patterns") {
    val notPatterns = List(
      "*[self::a or self::b][p(.)]/c/descendant-or-self::c", // taken from http://stackoverflow.com/a/9071744
      "boolean('')"
    )

    notPatterns.foreach(p => {
      val parsed = XPathParser.parse(p)
      assert(!XPathExpr.isPattern(parsed), f"$p must not be recognized as valid pattern")
    })
  }

  test("Default priorities") {
    // this list is taken from http://www.lenzconsulting.com/how-xslt-works/
    val patterns = List[(Double, String)](
      (-0.5, "*"),
      (-0.5, "@*"),
      (-0.5, "node()"),
      (-0.5, "text()"),
      (-0.5, "comment()"),
      //(-0.5, "processing-instruction()"), NOT SUPPORTED
      //(-0.25, "xyz:*"), NOT SUPPORTED
      //(-0.25, "@xyz:*"), NOT SUPPORTED
      (0, "foo"),
      //(0, "xyz:foo"), NOT SUPPORTED
      (0, "@foo"),
      //(0, "@xyz:foo"), NOT SUPPORTED
      //(0, "processing-instruction('foo')"), NOT SUPPORTED
      (0.5, "/"),
      (0.5, "/foo"),
      (0.5, "foo/bar"),
      (0.5, "foo[2]"),
      (0.5, "/foo[bar='bat']")
    )

    patterns.foreach { case (prio, pat) => {
      val parsed = XPathParser.parse(pat)
      assert(XPathExpr.isPattern(parsed))
      assert(parsed.isInstanceOf[LocationPath])
      assertResult(prio, f"Wrong priority for pattern $pat") {
        XPathExpr.getDefaultPriority(parsed.asInstanceOf[LocationPath])
      }
    }}
  }
}
