package xpath

/** An XPath location path step (see XPath spec section 2.1) */
case class XPathStep(axis: XPathAxis, test: NodeTest, predicates: Seq[XPathExpr])

/** Base class for node tests (see XPath spec section 2.3) */
abstract class NodeTest
case object AllNodeTest extends NodeTest
case object CommentNodeTest extends NodeTest
case object TextNodeTest extends NodeTest
case class NameTest(name: String) extends NodeTest