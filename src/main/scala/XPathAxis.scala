import org.jaxen.saxpath.Axis

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

object XPathAxis {
  def parse(axis: Int) : XPathAxis = axis match {
    case Axis.CHILD => ChildAxis
    case Axis.DESCENDANT => DescendantAxis
    case Axis.PARENT => ParentAxis
    case Axis.FOLLOWING_SIBLING => FollowingSiblingAxis
    case Axis.PRECEDING_SIBLING => PrecedingSiblingAxis
    case Axis.FOLLOWING => FollowingAxis
    case Axis.PRECEDING => PrecedingAxis
    case Axis.ATTRIBUTE => AttributeAxis
    case Axis.NAMESPACE => NamespaceAxis
    case Axis.SELF => SelfAxis
    case Axis.DESCENDANT_OR_SELF => DescendantOrSelfAxis
    case Axis.ANCESTOR_OR_SELF => AncestorOrSelfAxis
    case Axis.ANCESTOR => AncestorAxis
  }
}
