package analysis

import analysis.domain.XMLDomain
import xpath._

import scala.collection.mutable.{MutableList => MutList}

class AbstractPatternMatcher[N, L, V](xmlDom: XMLDomain[N, L, V]) {

  /** Returns a value indicating whether a given node matches a location path pattern.
    * NOTE: only supports location path patterns (XSLT spec section 5.2) without predicates
    *
    * First result: nodes for which the path WILL definitely match. When this is BOTTOM it means
    *               that the path can't match the given input.
    * Second result: nodes for which the path WILL definitely NOT match. When this is BOTTOM it
    *                means that the path will always match the given input.
    */
  def matches(node: N, path: LocationPath): (N, N) =
    matches(node, path.steps.reverse, path.isAbsolute)

  private def matches(node: N, reversedPathSteps: List[XPathStep], pathIsAbsolute: Boolean): (N, N) = {
    // match recursively from right to left (path steps are reversed!)
    reversedPathSteps match {
      case nextStep :: restPath =>
        val (nextStepMatches, notNextStepMatches) = matchesSingleStep(node, nextStep)
        if (!xmlDom.lessThanOrEqual(nextStepMatches, xmlDom.bottom)) restPath match {
          case XPathStep(DescendantOrSelfAxis, AllNodeTest, Nil) :: Nil =>
            // special case for the trivial '//' step as leftmost step in the path, which always matches
            // because every node is a descendant of the root node
            assert(pathIsAbsolute)
            (nextStepMatches, notNextStepMatches)
          case XPathStep(DescendantOrSelfAxis, AllNodeTest, Nil) :: restTail =>
            // the next step is '//' and must be handled separately (find nodes where some ancestor matches the rest of the path)
            var current = nextStepMatches
            var ancestorMatches = xmlDom.bottom
            var notAncestorMatches = node
            var (root, notRoot) = xmlDom.isRoot(current)
            val nodeStack = MutList(notRoot)
            while (!xmlDom.lessThanOrEqual(notRoot, xmlDom.bottom)) {
              // NOTE: This loop will not terminate when one can always move to the next parent, then
              //       the parent of the parent, ... infinitely.
              //       A better approach would search for a fixed point somewhere, but non-trivial '//'-steps
              //       were never used in our tested real-life XSLT stylesheets
              val parent = xmlDom.getParent(notRoot)
              val (parentMatchesRest, _) = matches(parent, restTail, pathIsAbsolute)
              var parentMatches = parentMatchesRest

              for (d <- (nodeStack.size - 1) to 1 by -1) {
                // go down in the tree, back to the original level
                val (newParentMatches, _) = hasParent(nodeStack(d), parentMatches)
                parentMatches = newParentMatches
              }

              val (newParentMatches, newNotParentMatches) = hasParent(nodeStack(0), parentMatches)
              ancestorMatches = xmlDom.join(ancestorMatches, newParentMatches)
              notAncestorMatches = xmlDom.meet(notAncestorMatches, newNotParentMatches)

              current = parent
              val (newRoot, newNotRoot) = xmlDom.isRoot(current)
              root = newRoot
              notRoot = newNotRoot
              nodeStack += notRoot
            }
            (ancestorMatches, xmlDom.join(notAncestorMatches, notNextStepMatches))
          case _ =>
            // find nodes where the parent matches the rest of the path
            val parent = xmlDom.getParent(nextStepMatches)
            val (parentMatchesRest, _) = matches(parent, restPath, pathIsAbsolute)
            val (parentMatches, notParentMatches) = hasParent(nextStepMatches, parentMatchesRest)
            (parentMatches, xmlDom.join(notParentMatches, notNextStepMatches))
        } else (xmlDom.bottom, notNextStepMatches)
      case Nil =>
        // an empty path is always a match, except when the path is absolute and the current node is not a root node
        if (pathIsAbsolute) xmlDom.isRoot(node) else (node, xmlDom.bottom)
    }
  }

  /** Predicate function that checks whether a node has a specified node as its parent.
    * The first result is a node that is known to have that parent (this is BOTTOM if the node definitely
    * doesn't have that parent), the second result is a node that might not have that parent (this is
    * BOTTOM if the node definitely does have that parent). The two results are not necessarily disjoint.
    */
  private def hasParent(node: N, parent: N): (N, N) = {
    val (isChild, isNotChild) = xmlDom.isContainedIn(node, xmlDom.getChildren(parent))
    val (isAttribute, isNeither) = xmlDom.isContainedIn(isNotChild, xmlDom.getAttributes(parent))
    (xmlDom.join(isChild, isAttribute), isNeither)
  }

  private def matchesSingleStep(node: N, step: XPathStep): (N, N) = step match {
    // child::node()
    case XPathStep(ChildAxis, AllNodeTest, Nil) =>
      // NOTE: we assume here that there are no more node types that element, text, comment, root and attribute
      val matches = xmlDom.joinAll(List(xmlDom.isElement(node)._1, xmlDom.isTextNode(node)._1, xmlDom.isComment(node)._1))
      val notMatches = xmlDom.join(xmlDom.isRoot(node)._1, xmlDom.isAttribute(node)._1)
      (matches, notMatches)
    // child::comment()
    case XPathStep(ChildAxis, CommentNodeTest, Nil) => xmlDom.isComment(node)
    // child::text()
    case XPathStep(ChildAxis, TextNodeTest, Nil) => xmlDom.isTextNode(node)
    // child::*
    case XPathStep(ChildAxis, NameTest(None, "*"), Nil) => xmlDom.isElement(node)
    // child::name
    case XPathStep(ChildAxis, NameTest(None, name), Nil) =>
      val (element, notElement) = xmlDom.isElement(node)
      val (hasName, notHasName) = xmlDom.hasName(element, name)
      (hasName, xmlDom.join(notElement, notHasName))
    // attribute::* OR attribute::node()
    case XPathStep(AttributeAxis, NameTest(None, "*") | AllNodeTest, Nil) => xmlDom.isAttribute(node)
    // attribute::name
    case XPathStep(AttributeAxis, NameTest(None, name), Nil) =>
      val (attr, notAttr) = xmlDom.isAttribute(node)
      val (hasName, notHasName) = xmlDom.hasName(attr, name)
      (hasName, xmlDom.join(notAttr, notHasName))
    // attribute::comment() OR attribute::text() [these can never match anything]
    case XPathStep(AttributeAxis, CommentNodeTest | TextNodeTest, _) => (xmlDom.bottom, node)
    // any step using a name test with a prefixed name or a non-empty list of predicates
    case XPathStep(_, NameTest(Some(_), _), Nil) => throw new NotImplementedError("Prefixed names are not implemented")
    case XPathStep(_, _, _ :: _) => throw new NotImplementedError("Predicates in paths are not implemented")
  }
}

