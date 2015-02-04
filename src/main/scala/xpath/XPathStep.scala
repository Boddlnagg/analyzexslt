package xpath

/** An XPath location path step (see XPath spec section 2.1) */
case class XPathStep(axis: XPathAxis, test: NodeTest, predicates: Seq[XPathExpr])

/** Base class for node tests (see XPath spec section 2.3) */
abstract class NodeTest
case object AllNodeTest extends NodeTest
case object CommentNodeTest extends NodeTest
case object TextNodeTest extends NodeTest
case class NameTest(prefix: Option[String], name: String) extends NodeTest
case class ProcessingInstructionTest(name: Option[String]) extends NodeTest

object XPathStep {
  /** Check whether the given step is equivalent to `/descendant-or-self::node()/`, which is the same as `//` */
  def isDescendantSelector(step: XPathStep): Boolean = step match {
    case XPathStep(DescendantOrSelfAxis, AllNodeTest, Nil) => true
    case XPathStep(_, _, Nil) => false
    case _ => throw new NotImplementedError("Predicates are not implemented.")
  }
}