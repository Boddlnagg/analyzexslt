package analysis.domain.path

import analysis._
import analysis.domain.XMLDomain

object XPathPatternDomain {
  type N = Option[Set[XPathPattern]]
  type L = Option[Set[XPathPattern]] // TODO: add optional length of list
  type V = Unit

  implicit class Crossable[X](xs: Traversable[X]) {
    def cross[Y](ys: Traversable[Y]) = for { x <- xs; y <- ys } yield (x, y)
  }

  /** This is the actual domain implementation */
  object D extends XMLDomain[N, L, V] {
    /** Get the TOP element for XML nodes. */
    def top: N = None

    /** Gets the BOTTOM element for XML nodes. */
    def bottom: N = BOT

    private val BOT: N = Some(Set())

    /** Get the TOP element for XML node lists.*/
    def topList: L = ???

    /** Gets the BOTTOM element for XML node lists. */
    def bottomList: L = ???

    /** Join two nodes. This calculates their supremum (least upper bound). */
    def join(n1: N, n2: N): N = (n1, n2) match {
      case (None, _) => None
      case (_, None) => None
      case (BOT, _) => n2
      case (_, BOT) => n1
      case (Some(s1), Some(s2)) => Some(s1.union(s2).toList.foldRight(List[XPathPattern]()) {
        case (next, acc) => if (acc.exists(e => lessThanOrEqualSingle(Some(next), Some(e))))
          acc
        else
          next :: acc.filter(e => !lessThanOrEqualSingle(Some(e), Some(next)))
      }.toSet)
    }

    /** Join two node lists. This calculates their supremum (least upper bound). */
    def joinList(l1: L, l2: L): L = ???

    /** Compares two elements of the lattice of nodes.
      * TOP is always greater than everything else, BOTTOM is always less than everything else.
      */
    override def compare(n1: N, n2: N): LatticeOrdering = (lessThanOrEqual(n1, n2), lessThanOrEqual(n2, n1)) match {
      case (true, true) => Equal
      case (true, false) => Less
      case (false, true) => Greater
      case (false, false) => Incomparable
    }

    def lessThanOrEqual(n1: N, n2: N): Boolean = (n1, n2) match {
      case (_, None) => true
      case (None, Some(_)) => false
      case (Some(s1), Some(s2)) => s1.forall(pat1 => s2.exists(pat2 => lessThanOrEqualSingle(Some(pat1), Some(pat2))))
    }

    def lessThanOrEqualSingle(left: Option[XPathPattern], right: Option[XPathPattern]): Boolean = (left, right) match {
      case (_, None) => true
      case (None, Some(_)) => false
      case (Some(pat1), Some(pat2)) => (pat1, pat2) match {
        case (Root, Root) => true
        case (AnyElement(prev1), AnyElement(prev2)) => lessThanOrEqualSingle(prev1, prev2)
        case (NamedElement(name1, prev1), NamedElement(name2, prev2)) if name1 == name2 => lessThanOrEqualSingle(prev1, prev2)
        case (NamedElement(_, prev1), AnyElement(prev2)) => lessThanOrEqualSingle(prev1, prev2)
        case (AnyAttribute(prev1), AnyAttribute(prev2)) => lessThanOrEqualSingle(prev1, prev2)
        case (NamedAttribute(name1, prev1), NamedAttribute(name2, prev2)) if name1 == name2 => lessThanOrEqualSingle(prev1, prev2)
        case (NamedAttribute(_, prev1), AnyAttribute(prev2)) => lessThanOrEqualSingle(prev1, prev2)
        // TODO: add recursive cases for other node types
        case _ => false
      }
    }

    /** Compares two elements of the lattice of node lists.
      * TOP is always greater than everything else, BOTTOM is always less than everything else.
      */
    def compareList(l1: L, l2: L): LatticeOrdering = ???

    /** Create an element node with the given name, attributes and children.
      * The output is created bottom-up, so children are always created before their parent nodes.
      */
    def createElement(name: String, attributes: L, children: L): N = ???

    /** Create an attribute node with the given name and text value.
      * Values that are not strings evaluate to BOTTOM.
      */
    def createAttribute(name: String, value: V): N = ???

    /** Create a text node with the given text value.
      * Values that are not strings evaluate to BOTTOM.
      */
    def createTextNode(value: V): N = ???

    /** Create an emtpy list containing no nodes */
    def createEmptyList(): L = ???

    /** Create a list containing a single abstract node */
    def createSingletonList(node: N): L = ???

    /** Get the root node of a given node */
    def getRoot(node: N): N = ???

    /** Get the list of attributes of a given node.
      * Nodes that are not an element (and therefore don't have attributes) return an empty list, not BOTTOM! */
    def getAttributes(node: N): L = node match {
      case None => None
      case Some(s) => Some(s.map(e => AnyAttribute(Some(e))))
    }

    /** Get the list of children of a given node.
      * Root nodes have a single child, element nodes have an arbitrary number of children.
      * Nodes that don't have children return an empty list, not BOTTOM! */
    def getChildren(node: N): L = node match {
      case None => None
      case Some(s) => Some(s.map(e => AnyElement(Some(e))))
    }

    /** Get the parent of given node. */
    def getParent(node: N): N = node match {
      case None => Some(Set(AnyElement(None), Root))
      case Some(s) => join(s.toList.filter(e => e != Root).map { e =>
          e.prev match {
            case None => Some(Set[XPathPattern](AnyElement(None), Root))
            case Some(p) => Some(Set[XPathPattern](p))
          }
        })
    }

    /** Predicate function that checks whether a node has a specified node as its parent.
      * The first result is a node that is known to have that parent (this is BOTTOM if the node definitely
      * doesn't have that parent), the second result is a node that might not have that parent (this is
      * BOTTOM if the node definitely does have that parent). The two results are not necessarily disjoint.
      */
    def hasParent(node: N, parent: N): (N, N) = (node, parent) match {
      case (BOT, _) => (BOT, BOT)
      case (_, BOT) => (BOT, node) // parent is BOTTOM -> can't match
      case (None, _) => (None, None) // don't know anything about the node
      case (Some(_), None) => (node, node) // parent is TOP -> don't know anything
      case ((Some(nodes), Some(parents))) =>
        // TODO: The following assumption is wrong. We need to apply a meet operator for `prev`
        assert(nodes.forall(n => n.prev == None)) // nodes shouldn't have `prev` set yet
        val validParents = parents.filter {
          case Root => true
          case AnyElement(_) => true
          case NamedElement(_, _) => true
          case _ => false
        }
        (Some(nodes.cross(validParents).map {
          case (n, p) => n.withPrev(p)
        }.toSet), node)
    }

    /** Predicate function that checks whether a node has a specified node as its ancestor.
      * The first result is a node that is known to have that ancestor (this is BOTTOM if the node definitely
      * doesn't have that ancestor), the second result is a node that might not have that ancestor (this is
      * BOTTOM if the node definitely does have that ancestor). The two results are not necessarily disjoint.
      */
    def hasAncestor(node: N, ancestor: N): (N, N) = ???

    /** Concatenates two lists. */
    def concatLists(list1: L, list2: L): L = ???

    /** Partitions a node list in such a way that the first result contains all attribute nodes from the beginning of
      * the list (as soon as there are other node types in the list, attributes are ignored) and the second result
      * contains all other nodes.
      */
    def partitionAttributes(list: L): (L, L) = ???

    /** Wraps a list of nodes in a document/root node. Lists that don't have exactly one element evaluate to BOTTOM. */
    def wrapInRoot(list: L): N = ???

    /** Copies a list of nodes, so that they can be used in the output.
      * A root node is copied by copying its child (not wrapped in a root node). */
    def copyToOutput(list: L): L = ???

    /** Evaluates a function for every element in the given list, providing also the index of each element in the list.
      * The resulting lists are flattened into a single list.
      */
    def flatMapWithIndex(list: L, f: (N, V) => L): L = ???

    /** Gets the size of a node list */
    def getNodeListSize(list: L): V = ???

    /** Gets the string-value of a node, as specified in the XSLT specification */
    def getStringValue(node: N): V = ???

    /** Predicate function that checks whether a node is a root node.
      * The first result is a node that is known to be a root node (this is BOTTOM if the node definitely
      * is not a root node), the second result is a node that might not be a root node (this is
      * BOTTOM if the node definitely is a root node). The two results are not necessarily disjoint.
      */
    def isRoot(node: N): (N, N) = {
      val rootSet: Set[XPathPattern] = Set(Root)
      node match
      {
        case None => (Some(rootSet), node)
        case Some(s) => (Some(s.intersect(rootSet)), Some(s.diff(rootSet)))
      }
    }

    /** Predicate function that checks whether a node is an element node.
      * The first result is a node that is known to be an element node (this is BOTTOM if the node definitely
      * is not an element node), the second result is a node that might not be an element node (this is
      * BOTTOM if the node definitely is an element node). The two results are not necessarily disjoint.
      */
    def isElement(node: N): (N, N) = node match {
      case None => (Some(Set(AnyElement(None))), node)
      case Some(s) => (Some(s.collect {
        case e@NamedElement(_, _) => e
        case e@AnyElement(_) => e
      }), node)
    }

    /** Predicate function that checks whether a node is a text node.
      * The first result is a node that is known to be a text node (this is BOTTOM if the node definitely
      * is not a text node), the second result is a node that might not be a text node (this is
      * BOTTOM if the node definitely is a text node). The two results are not necessarily disjoint.
      */
    def isTextNode(node: N): (N, N) = ???

    /** Predicate function that checks whether a node is a comment node.
      * The first result is a node that is known to be a comment node (this is BOTTOM if the node definitely
      * is not a comment node), the second result is a node that might not be a comment node (this is
      * BOTTOM if the node definitely is a comment node). The two results are not necessarily disjoint.
      */
    def isComment(node: N): (N, N) = ???

    /** Predicate function that checks whether a node is an attribute node.
      * The first result is a node that is known to be an attribute node (this is BOTTOM if the node definitely
      * is not an attribute node), the second result is a node that might not be an attribute node (this is
      * BOTTOM if the node definitely is an attribute node). The two results are not necessarily disjoint.
      */
    def isAttribute(node: N): (N, N) = node match {
      case None => (Some(Set(AnyAttribute(None))), node)
      case Some(s) => (Some(s.collect {
        case e@NamedAttribute(_, _) => e
        case e@AnyAttribute(_) => e
      }), node)
    }

    /** Predicate function that checks whether a node has a specified name.
      * The first result is a node that is known to have that name (this is BOTTOM if the node definitely
      * doesn't have that name), the second result is a node that might not have that name (this is
      * BOTTOM if the node definitely does have that name). The two results are not necessarily disjoint.
      * Nodes that don't have a name (any node except element and attribute nodes) are evaluated to BOTTOM.
      */
    def hasName(node: N, name: String): (N, N) = node match {
      case None => (Some(Set(NamedElement(name, None), NamedAttribute(name, None))), node)
      case Some(s) => (Some(s.collect {
        case e@NamedElement(n, _) if (name == n) => e
        case AnyElement(p) => NamedElement(name, p)
        case a@NamedAttribute(n, _) if (name == n) => a
        case AnyAttribute(p) => NamedAttribute(name, p)
      }), node)
    }

    /** Get the name for a given node. Nodes that don't have a name (i.e. are not an element or attribute node)
      * are evaluated to the empty string, not BOTTOM!
      */
    def getNodeName(node: N): V = ???

    /** Concatenates the values of all text nodes in the list. List elements that are not text nodes are ignored. */
    def getConcatenatedTextNodeValues(list: L): V = ???

    /** Filters a list using a given predicate function. The predicate function should never return a node
      * (as its first result) that is less precise than the input node.
      */
    def filter(list: L, predicate: N => (N, N)): L = ???
  }
}