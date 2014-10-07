package xpath

import xml._

object XPathMatcher {
  /** Returns a value indicating whether a given node matches a location path pattern.
    * NOTE: only supports location path patterns (XSLT spec section 5.2) without predicates
    */
  def matches(node: XMLNode, path: LocationPath): Boolean = {

    // match recursively from right to left
    if (path.steps.isEmpty) {
      // an empty path is always a match, but when it is an absolute path, the current node must be the root node
      if (path.isAbsolute) node.isInstanceOf[XMLRoot] else true
    } else {
      val lastStep = path.steps.last
      val restPath = LocationPath(path.steps.dropRight(1), path.isAbsolute)
      if (!lastStep.predicates.isEmpty) throw new NotImplementedError("predicates in paths are not implemented")
      val lastStepMatches = lastStep match {
        // child::node() OR attribute::node()
        // this matches any node (regardless of axis type) according to spec section 2.3
        case XPathStep(ChildAxis | AttributeAxis, AllNodeTest, Nil) => true
        // child::comment()
        case XPathStep(ChildAxis, CommentNodeTest, Nil) => node.isInstanceOf[XMLComment]
        // child::text()
        case XPathStep(ChildAxis, TextNodeTest, Nil) => node.isInstanceOf[XMLTextNode]
        // child::*
        case XPathStep(ChildAxis, NameTest("*"), Nil) => node.isInstanceOf[XMLElement]
        // child::name
        case XPathStep(ChildAxis, NameTest(name), Nil) => node.isInstanceOf[XMLElement] && node.asInstanceOf[XMLElement].name == name
        // attribute::*
        case XPathStep(AttributeAxis, NameTest("*"), Nil) => node.isInstanceOf[XMLAttribute]
        // attribute::name
        case XPathStep(AttributeAxis, NameTest(name), Nil) => node.isInstanceOf[XMLAttribute] && node.asInstanceOf[XMLAttribute].name == name
        // attribute::comment() OR attribute::text()
        // these can never match anything
        case XPathStep(AttributeAxis, CommentNodeTest | TextNodeTest, _) => false
      }

      if (!lastStepMatches) {
        false
      } else {
        if (!restPath.steps.isEmpty && restPath.steps.last == XPathStep(DescendantOrSelfAxis, AllNodeTest, Nil)) {
          // the next step is '//' and must be handled separately (does any ancestor match the rest of the path?)
          val nextRestPath = LocationPath(restPath.steps.dropRight(1), path.isAbsolute)
          node.ancestors.exists(a => matches(a, nextRestPath))
        }
        else
          matches(node.parent, restPath) // does the parent match the rest of the path?
      }
    }
  }
}
