package xslt

import xml._
import xpath._

object XSLTPatternMatcher {
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
      if (lastStep.predicates.nonEmpty) throw new NotImplementedError("predicates in paths are not implemented")
      val lastStepMatches = lastStep match {
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
        // any step using a name test with a prefixed name
        case XPathStep(_, NameTest(Some(_), _), Nil) => throw new NotImplementedError("Prefixed names are not implemented")
      }

      if (!lastStepMatches) {
        false
      } else {
        if (restPath.steps.nonEmpty && restPath.steps.last == XPathStep(DescendantOrSelfAxis, AllNodeTest, Nil)) {
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