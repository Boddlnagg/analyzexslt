package xpath

import xml._

/** Base class for XPath axes (see XPath spec section 2.2) */
abstract class XPathAxis
case object ChildAxis extends XPathAxis
case object DescendantAxis extends XPathAxis
case object ParentAxis extends XPathAxis
case object FollowingSiblingAxis extends XPathAxis
case object PrecedingSiblingAxis extends XPathAxis
case object FollowingAxis extends XPathAxis
case object PrecedingAxis extends XPathAxis
case object AttributeAxis extends XPathAxis
case object NamespaceAxis extends XPathAxis
case object SelfAxis extends XPathAxis
case object DescendantOrSelfAxis extends XPathAxis
case object AncestorOrSelfAxis extends XPathAxis
case object AncestorAxis extends XPathAxis

/** Helper functions for XPath axes. */
object XPathAxis {
  /** Returns a value indicating whether a node is of the principal node type of a given axis (see XPath spec section 2.3) */
  def isPrincipalNodeType(axis: XPathAxis, node: XMLNode): Boolean = {
    axis match {
      case AttributeAxis => node.isInstanceOf[XMLAttribute]
      case NamespaceAxis => false // namespace nodes are not supported
      case _ => node.isInstanceOf[XMLElement]
    }
  }
}
