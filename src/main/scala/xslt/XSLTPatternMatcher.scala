package xslt

import xml._
import xpath._

object XSLTPatternMatcher {
  /** Returns a value indicating whether a given node matches a location path pattern.
    * NOTE: only supports location path patterns (XSLT spec section 5.2) without predicates
    */
  def matches(node: XMLNode, path: LocationPath): Boolean =
    matches(node, path.steps.reverse, path.isAbsolute)

  private def matches(node: XMLNode, reversedPathSteps: List[XPathStep], pathIsAbsolute: Boolean): Boolean = {
    // match recursively from right to left (path steps are reversed!)
    reversedPathSteps match {
      case nextStep :: restPath =>
        if (matchesSingleStep(node, nextStep)) {
          restPath match {
            case XPathStep(DescendantOrSelfAxis, AllNodeTest, Nil) :: Nil =>
              // special case for the trivial '//' step as leftmost step in the path, which always matches
              // because every node is a descendant of the root node
              assert(pathIsAbsolute)
              true
            case XPathStep(DescendantOrSelfAxis, AllNodeTest, Nil) :: restTail =>
              // the next step is '//' and must be handled separately:
              // does any ancestor match the rest of the path?
              node.ancestors.exists(a => matches(a, restTail, pathIsAbsolute))
            case _ =>
              // does the parent match the rest of the path?
              matches(node.parent, restPath, pathIsAbsolute)
          }
        } else { false }
      case Nil =>
        // an empty path is always a match, except when the path is absolute and the current node is not a root node
        if (pathIsAbsolute) node.isInstanceOf[XMLRoot] else true
    }
  }

  private def matchesSingleStep(node: XMLNode, step: XPathStep) = step match {
    // child::node()
    case XPathStep(ChildAxis, AllNodeTest, Nil) => node.isInstanceOf[XMLElement] | node.isInstanceOf[XMLTextNode] | node.isInstanceOf[XMLComment]
    // child::comment()
    case XPathStep(ChildAxis, CommentNodeTest, Nil) => node.isInstanceOf[XMLComment]
    // child::text()
    case XPathStep(ChildAxis, TextNodeTest, Nil) => node.isInstanceOf[XMLTextNode]
    // child::*
    case XPathStep(ChildAxis, NameTest(None, "*"), Nil) => node.isInstanceOf[XMLElement]
    // child::name
    case XPathStep(ChildAxis, NameTest(None, name), Nil) => node.isInstanceOf[XMLElement] && node.asInstanceOf[XMLElement].name == name
    // attribute::*
    case XPathStep(AttributeAxis, NameTest(None, "*") | AllNodeTest, Nil) => node.isInstanceOf[XMLAttribute]
    // attribute::name
    case XPathStep(AttributeAxis, NameTest(None, name), Nil) => node.isInstanceOf[XMLAttribute] && node.asInstanceOf[XMLAttribute].name == name
    // attribute::comment() OR attribute::text() [these can never match anything]
    case XPathStep(AttributeAxis, CommentNodeTest | TextNodeTest, _) => false
    // any step using a name test with a prefixed name or a non-empty list of predicates
    case XPathStep(_, NameTest(Some(_), _), Nil) => throw new NotImplementedError("Prefixed names are not implemented")
    case XPathStep(_, _, _ :: _) => throw new NotImplementedError("Predicates in paths are not implemented")
  }
}
