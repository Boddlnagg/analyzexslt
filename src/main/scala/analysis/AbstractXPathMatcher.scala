package analysis

import analysis.domain.XMLDomain
import xpath._

import scala.collection.mutable.MutableList

class AbstractXPathMatcher[N, L, V](xmlDom: XMLDomain[N, L, V]) {

  /** Returns a value indicating whether a given node matches a location path pattern.
    * NOTE: only supports location path patterns (XSLT spec section 5.2) without predicates
    */
  def matches(node: N, path: LocationPath): (N, N) = {
    // first result: nodes for which the path WILL definitely match. When this is BOTTOM it means that the path can't match the given input.
    // second result: nodes for which the path WILL definitely NOT match. When this is BOTTOM it means that the path will always match the given input.

    // match recursively from right to left
    if (path.steps.isEmpty) {
      // an empty path is always a match, but when it is an absolute path, the current node must be the root node
      if (path.isAbsolute) xmlDom.isRoot(node) else (node, xmlDom.bottom)
    } else {
      val lastStep = path.steps.last
      val restPath = LocationPath(path.steps.dropRight(1), path.isAbsolute)
      if (!lastStep.predicates.isEmpty) throw new NotImplementedError("predicates in paths are not implemented")
      val (lastStepMatches, notLastStepMatches) = lastStep match {
        // child::node()
        case XPathStep(ChildAxis, AllNodeTest, Nil) =>
          val matches = xmlDom.join(List(xmlDom.isElement(node)._1, xmlDom.isTextNode(node)._1, xmlDom.isComment(node)._1))
          val notMatches = xmlDom.join(xmlDom.isRoot(node)._1, xmlDom.isAttribute(node)._1) // TODO: is this always correct?
          (matches, notMatches)
        // child::comment()
        case XPathStep(ChildAxis, CommentNodeTest, Nil) => xmlDom.isComment(node)
        // child::text()
        case XPathStep(ChildAxis, TextNodeTest, Nil) => xmlDom.isTextNode(node)
        // child::*
        case XPathStep(ChildAxis, NameTest("*"), Nil) => xmlDom.isElement(node)
        // child::name
        case XPathStep(ChildAxis, NameTest(name), Nil) =>
          val (element, notElement) = xmlDom.isElement(node)
          val (hasName, notHasName) = xmlDom.hasName(element, name)
          (hasName, xmlDom.join(notElement, notHasName))
        // attribute::* OR attribute::node()
        case XPathStep(AttributeAxis, NameTest("*") | AllNodeTest, Nil) => xmlDom.isAttribute(node)
        // attribute::name
        case XPathStep(AttributeAxis, NameTest(name), Nil) =>
          val (attr, notAttr) = xmlDom.isAttribute(node)
          val (hasName, notHasName) = xmlDom.hasName(attr, name)
          (hasName, xmlDom.join(notAttr, notHasName))
        // attribute::comment() OR attribute::text()
        // these can never match anything
        case XPathStep(AttributeAxis, CommentNodeTest | TextNodeTest, _) => (xmlDom.bottom, node)
      }

      if (xmlDom.lessThanOrEqual(lastStepMatches, xmlDom.bottom)) {
        (xmlDom.bottom, notLastStepMatches)
      } else {
        // this node could match, but what about the rest of the path?
        if (!restPath.steps.isEmpty && restPath.steps.last == XPathStep(DescendantOrSelfAxis, AllNodeTest, Nil)) {
          // the next step is '//' and must be handled separately (does any ancestor match the rest of the path?)
          val nextRestPath = LocationPath(restPath.steps.dropRight(1), path.isAbsolute)
          var current = lastStepMatches
          var ancestorMatches = xmlDom.bottom
          var notAncestorMatches = node
          var (root, notRoot) = xmlDom.isRoot(current)
          val nodeStack = MutableList(notRoot)
          while (xmlDom.compare(notRoot, xmlDom.bottom) == Greater) {
            // TODO: this may not terminate (make sure that domains can not have infinite chains of parents or add stack?)
            val parent = xmlDom.getParent(notRoot)
            val (parentMatchesRest, _) = matches(parent, nextRestPath)
            var parentMatches = parentMatchesRest

            for (d <- (nodeStack.size - 1) to 1 by -1) {
              // go down in the tree, back to the original level
              val (newParentMatches, _) = xmlDom.hasParent(nodeStack(d), parentMatches)
              parentMatches = newParentMatches
            }

            val (newParentMatches, newNotParentMatches) = xmlDom.hasParent(nodeStack(0), parentMatches)
            ancestorMatches = xmlDom.join(ancestorMatches, newParentMatches)
            notAncestorMatches = xmlDom.join(notAncestorMatches, newNotParentMatches) // TODO: this should probably be MEET instead of JOIN

            current = parent
            val (newRoot, newNotRoot) = xmlDom.isRoot(current) // instead of returning two results here, one could add isNotRoot
            root = newRoot
            notRoot = newNotRoot
            nodeStack += notRoot
          }
          (ancestorMatches, xmlDom.join(notAncestorMatches, notLastStepMatches))
        }
        else {
          // does the parent match the rest of the path?
          val parent = xmlDom.getParent(lastStepMatches)
          val (parentMatchesRest, _) = matches(parent, restPath)
          val (parentMatches, notParentMatches) = xmlDom.hasParent(lastStepMatches, parentMatchesRest)
          (parentMatches, xmlDom.join(notParentMatches, notLastStepMatches))
        }
      }
    }
  }
}

