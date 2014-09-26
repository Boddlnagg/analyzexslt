package analysis

import analysis.domain.Domain
import xml._
import xpath._

class AbstractXPathMatcher[N, L, V](dom: Domain[N, L, V]) {
  val xmlDom = dom.xmlDom
  val xpathDom = dom.xpathDom

  /** Returns a value indicating whether a given node matches a location path pattern.
    * NOTE: only supports location path patterns (XSLT spec section 5.2) without predicates
    */
  def matches(node: N, path: LocationPath): (N, N) = {

    // match recursively from right to left
    if (path.steps.isEmpty) {
      // an empty path is always a match, but when it is an absolute path, the current node must be the root node
      if (path.isAbsolute) xmlDom.isRoot(node) else (node, xmlDom.bottom)
    } else {
      val lastStep = path.steps.last
      val restPath = LocationPath(path.steps.dropRight(1), path.isAbsolute)
      if (!lastStep.predicates.isEmpty) throw new NotImplementedError("predicates in paths are not implemented")
      lastStep match {
        // special handling of pattern '//'
        case XPathStep(DescendantOrSelfAxis, AllNodeTest, Nil) =>
          // does any ancestor match the rest of the path?
          throw new NotImplementedError("Matching of pattern containing `//` is not implemented.")
          // node.ancestors.exists(a => matches(a, restPath))
        case _ =>  val (lastStepMatches, notLastStepMatches) = lastStep match {
          // child::node() OR attribute::node()
          // this matches any node (regardless of axis type) according to spec section 2.3
          case XPathStep(ChildAxis | AttributeAxis, AllNodeTest, Nil) => (node, xmlDom.bottom)
          // child::comment()
          case XPathStep(ChildAxis, CommentNodeTest, Nil) => xmlDom.isComment(node)
          // child::text()
          case XPathStep(ChildAxis, TextNodeTest, Nil) => xmlDom.isTextNode(node)
          // child::*
          case XPathStep(ChildAxis, NameTest("*"), Nil) => xmlDom.isElement(node)
          // child::name
          case XPathStep(ChildAxis, NameTest(name), Nil) =>
            val (element, notElement) = xmlDom.isElement(node)
            val (hasName, notHasName) = xmlDom.nameMatches(element, name)
            (hasName, xmlDom.join(notElement, notHasName))
          // attribute::*
          case XPathStep(AttributeAxis, NameTest("*"), Nil) => xmlDom.isAttribute(node)
          // attribute::name
          case XPathStep(AttributeAxis, NameTest(name), Nil) =>
            val (attr, notAttr) = xmlDom.isElement(node)
            val (hasName, notHasName) = xmlDom.nameMatches(attr, name)
            (hasName, xmlDom.join(notAttr, notHasName))
          // attribute::comment() OR attribute::text()
          // these can never match anything
          case XPathStep(AttributeAxis, CommentNodeTest | TextNodeTest, _) => (xmlDom.bottom, node)
        }

          if (lastStepMatches == xmlDom.bottom) {
            (xmlDom.bottom, notLastStepMatches)
          } else {
            // this node COULD match, but does the parent match the rest of the path?
            // we have to check this for each part of N individually
            // TODO: this is currently wrong, because it returns the parent that matches instead of the node of which the parent matches ...
            val (restMatches, notRestMatches) = matches(xmlDom.getParent(lastStepMatches), restPath)
            (restMatches, xmlDom.join(notLastStepMatches, notRestMatches))
          }
      }
    }
  }
}

