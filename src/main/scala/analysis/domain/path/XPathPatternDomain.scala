package analysis.domain.path

import analysis.domain.XMLDomain

object XPathPatternDomain {
  type N = Set[XPathPattern]
  type L = Set[XPathPattern] // TODO: add optional length of list
  type V = Unit

  implicit class Crossable[X](xs: Traversable[X]) {
    def cross[Y](ys: Traversable[Y]) = for { x <- xs; y <- ys } yield (x, y)
  }

  /** This is the actual domain implementation */
  object D extends XMLDomain[N, L, V] {
    /** Get the TOP element for XML nodes. */
    override def top: N = Set(Root,
      DescendantStep(AnyElement, Root),
      DescendantStep(AnyAttribute, Root),
      DescendantStep(AnyCommentNode, Root),
      DescendantStep(AnyTextNode, Root))

    /** Gets the BOTTOM element for XML nodes. */
    override def bottom: N = Set()

    /** Get the TOP element for XML node lists.*/
    override def topList: L = ???

    /** Gets the BOTTOM element for XML node lists. */
    override def bottomList: L = ???

    /** Join two nodes. This calculates their supremum (least upper bound). */
    override def join(n1: N, n2: N): N = normalize(n1.union(n2))

    protected def normalize(set: Iterable[XPathPattern]): Set[XPathPattern] = set.toList.foldRight(List[XPathPattern]()) {
      case (next, acc) => if (acc.exists(e => lessThanOrEqualSingle(next, e)))
        acc
      else
        next :: acc.filter(e => !lessThanOrEqualSingle(e, next))
    }.toSet

    override def meet(n1: N, n2: N): N = {
      val result = n1.cross(n2).flatMap {
        case (p1, p2) => meetSingle(p1, p2)
      }
      normalize(result.toSet)
    }

    protected def meetSingle(left: XPathPattern, right: XPathPattern): Set[XPathPattern] = (left, right) match {
      case (Root, Root) => Set(Root)
      case (ChildStep(desc1, prev1), ChildStep(desc2, prev2)) =>
        meetSingle(prev1, prev2).flatMap { r =>
          meetDescriptor(desc1, desc2) match {
            case None => return Set() // BOTTOM
            case Some(desc) => Set(ChildStep(desc, r))
          }
        }
      case (DescendantStep(desc1, prev1), DescendantStep(desc2, prev2)) =>
        meetDescriptor(desc1, desc2) match {
          case None => Set()
          case Some(desc) => meetSingle(prev1, prev2).map(r => DescendantStep(desc, r)).asInstanceOf[Set[XPathPattern]] |
            meetSingle(DescendantStep(AnyElement, prev1), prev2).map(r => DescendantStep(desc, r)) |
            meetSingle(prev1, DescendantStep(AnyElement, prev2)).map(r => DescendantStep(desc, r))
        }
      case (DescendantStep(desc1, prev1), ChildStep(desc2, prev2)) =>
        meetDescriptor(desc1, desc2) match {
          case None => Set()
          case Some(desc) => meetSingle(prev1, prev2).map(r => ChildStep(desc, r)).asInstanceOf[Set[XPathPattern]] |
            meetSingle(DescendantStep(AnyElement, prev1), prev2).map(r => ChildStep(desc, r))
        }
      case (ChildStep(desc1, prev1), DescendantStep(desc2, prev2)) =>
        meetDescriptor(desc1, desc2) match {
          case None => Set()
          case Some(desc) => meetSingle(prev1, prev2).map(r => ChildStep(desc, r)).asInstanceOf[Set[XPathPattern]] |
            meetSingle(prev1, DescendantStep(AnyElement, prev2)).map(r => ChildStep(desc, r))
        }
      case _ => Set() // mix of Root and Step -> BOTTOM
    }

    protected def meetDescriptor(desc1: PatternStepDescriptor, desc2: PatternStepDescriptor): Option[PatternStepDescriptor] = {
      (desc1, desc2) match {
        case _ if desc1 == desc2 => Some(desc1)
        case (NamedElement(name), AnyElement) => Some(NamedElement(name))
        case (AnyElement, NamedElement(name)) => Some(NamedElement(name))
        case (NamedAttribute(name), AnyAttribute) => Some(NamedAttribute(name))
        case (AnyAttribute, NamedAttribute(name)) => Some(NamedAttribute(name))
        case _ => None // represents BOTTOM here
      }
    }

    /** Join two node lists. This calculates their supremum (least upper bound). */
    override def joinList(l1: L, l2: L): L = ???

    /** Compares two elements of the lattice of nodes.
      * Returns true if n1 < n2 or n1 = n2, false if n1 > n2 or if they are incomparable.
      */
    override def lessThanOrEqual(n1: N, n2: N): Boolean =
      n1.forall(pat1 => n2.exists(pat2 => lessThanOrEqualSingle(pat1, pat2)))

    protected def lessThanOrEqualSingle(left: XPathPattern, right: XPathPattern): Boolean = (left, right) match {
      case (Root, Root) => true
      case (ChildStep(desc1, prev1), ChildStep(desc2, prev2)) =>
        lessThanOrEqualDescriptor(desc1, desc2) && lessThanOrEqualSingle(prev1, prev2)
      case (DescendantStep(desc1, prev1), DescendantStep(desc2, prev2)) =>
        lessThanOrEqualDescriptor(desc1, desc2) && List(prev2, DescendantStep(AnyElement, prev2)).exists {
          p => lessThanOrEqualSingle(prev1, p)
        }
      case (ChildStep(desc1, prev1), DescendantStep(desc2, prev2)) =>
        lessThanOrEqualDescriptor(desc1, desc2) && List(prev2, DescendantStep(AnyElement, prev2)).exists {
          p => lessThanOrEqualSingle(prev1, p)
        }
      case (DescendantStep(desc1, prev1), ChildStep(desc2, prev2)) => false
      case _ => false // mix of Root and Step
    }

    protected def lessThanOrEqualDescriptor(desc1: PatternStepDescriptor, desc2: PatternStepDescriptor): Boolean = {
      (desc1, desc2) match {
        case _ if desc1 == desc2 => true // equal
        case (NamedElement(_), AnyElement) => true // less than
        case (NamedAttribute(_), AnyAttribute) => true // less than
        case _ => false
      }
    }

    /** Compares two elements of the lattice of node lists.
      * Returns true if l1 < l2 or l1 = l2, false if l1 > l2 or if they are incomparable.
      */
    override def lessThanOrEqualList(l1: L, l2: L): Boolean = ???

    /** Create an element node with the given name, attributes and children.
      * The output is created bottom-up, so children are always created before their parent nodes.
      */
    override def createElement(name: String, attributes: L, children: L): N = ???

    /** Create an attribute node with the given name and text value.
      * Values that are not strings evaluate to BOTTOM.
      */
    override def createAttribute(name: String, value: V): N = ???

    /** Create a text node with the given text value.
      * Values that are not strings evaluate to BOTTOM.
      */
    override def createTextNode(value: V): N = ???

    /** Create an emtpy list containing no nodes */
    override def createEmptyList(): L = ???

    /** Create a list containing a single abstract node */
    override def createSingletonList(node: N): L = ???

    /** Get the root node of a given node */
    override def getRoot(node: N): N = ???

    /** Get the list of attributes of a given node.
      * Nodes that are not an element (and therefore don't have attributes) return an empty list, not BOTTOM! */
    override def getAttributes(node: N): L = normalize(node.collect {
      // NOTE: only element nodes have attributes
      case e@ChildStep(AnyElement, _) => ChildStep(AnyAttribute, e)
      case e@ChildStep(NamedElement(_), _) => ChildStep(AnyAttribute, e)
      case e@DescendantStep(AnyElement, _) => ChildStep(AnyAttribute, e)
      case e@DescendantStep(NamedElement(_), _) => ChildStep(AnyAttribute, e)
      // TODO: other node types should return empty lists, which are currently not representable as L
    })

    /** Get the list of children of a given node.
      * Root nodes have a single child, element nodes have an arbitrary number of children.
      * Nodes that don't have children return an empty list, not BOTTOM! */
    override def getChildren(node: N): L = normalize(node.collect {
      case e@ChildStep(AnyElement, _) => List(ChildStep(AnyElement, e), ChildStep(AnyTextNode, e), ChildStep(AnyCommentNode, e))
      case e@ChildStep(NamedElement(_), _) => List(ChildStep(AnyElement, e), ChildStep(AnyTextNode, e), ChildStep(AnyCommentNode, e))
      case e@DescendantStep(AnyElement, _) => List(ChildStep(AnyElement, e), ChildStep(AnyTextNode, e), ChildStep(AnyCommentNode, e))
      case e@DescendantStep(NamedElement(_), _) => List(ChildStep(AnyElement, e), ChildStep(AnyTextNode, e), ChildStep(AnyCommentNode, e))
      case e@Root => List(ChildStep(AnyElement, e))
      // TODO: other node types (text, attribute) should return empty lists, which are currently not representable as L
    }.flatten)

    /** Get the parent of given node. If the node has no parent (root node), BOTTOM is returned. */
    override def getParent(node: N): N = normalize(node.toList.collect {
      case ChildStep(desc, prev) => List(prev)
      case DescendantStep(desc, prev) => prev match {
        case Root => desc match {
          // NOTE: only elements can have Root as their parent
          case AnyElement | NamedElement(_) => List(Root, DescendantStep(AnyElement, Root))
          case _ => List(DescendantStep(AnyElement, Root))
        }
        case p => List(p, DescendantStep(AnyElement, p))
      }
    }.flatten)

    /** Predicate function that checks whether a node is in a given list of nodes.
      * The first result is a node that is known to be in that list (this is BOTTOM if the node definitely
      * is not in the list), the second result is a node that might not be in the list (this is
      * BOTTOM if the node definitely is contained in the list). The two results are not necessarily disjoint.
      */
    override def isContainedIn(node: N, list: L): (N, N) = {
      def joinReduceList(list: L): N = list // this is a no-op

      val reducedList = joinReduceList(list)
      (meet(node, reducedList), node)
    }

    /** Concatenates two lists. */
    override def concatLists(list1: L, list2: L): L = ???

    /** Partitions a node list in such a way that the first result contains all attribute nodes from the beginning of
      * the list (as soon as there are other node types in the list, attributes are ignored) and the second result
      * contains all other nodes.
      */
    override def partitionAttributes(list: L): (L, L) = ???

    /** Wraps a list of nodes in a document/root node. Lists that don't have exactly one element evaluate to BOTTOM. */
    override def wrapInRoot(list: L): N = ???

    /** Copies a list of nodes, so that they can be used in the output.
      * A root node is copied by copying its child (not wrapped in a root node). */
    override def copyToOutput(list: L): L = ???

    /** Evaluates a function for every element in the given list, providing also the index of each element in the list.
      * The resulting lists are flattened into a single list.
      */
    override def flatMapWithIndex(list: L, f: (N, V) => L): L = ???

    /** Gets the size of a node list */
    override def getNodeListSize(list: L): V = ???

    /** Gets the string-value of a node, as specified in the XSLT specification */
    override def getStringValue(node: N): V = ???

    /** Predicate function that checks whether a node is a root node.
      * The first result is a node that is known to be a root node (this is BOTTOM if the node definitely
      * is not a root node), the second result is a node that might not be a root node (this is
      * BOTTOM if the node definitely is a root node). The two results are not necessarily disjoint.
      */
    override def isRoot(node: N): (N, N) = {
      val rootSet: Set[XPathPattern] = Set(Root)
      (meet(node, rootSet), normalize(node.diff(rootSet))) // TODO: revisit second result
    }

    /** Predicate function that checks whether a node is an element node.
      * The first result is a node that is known to be an element node (this is BOTTOM if the node definitely
      * is not an element node), the second result is a node that might not be an element node (this is
      * BOTTOM if the node definitely is an element node). The two results are not necessarily disjoint.
      */
    override def isElement(node: N): (N, N) = (normalize(node.collect {
      case e@ChildStep(NamedElement(_), _) => e
      case e@ChildStep(AnyElement, _) => e
      case e@DescendantStep(NamedElement(_), _) => e
      case e@DescendantStep(AnyElement, _) => e
    }), node)

    /** Predicate function that checks whether a node is a text node.
      * The first result is a node that is known to be a text node (this is BOTTOM if the node definitely
      * is not a text node), the second result is a node that might not be a text node (this is
      * BOTTOM if the node definitely is a text node). The two results are not necessarily disjoint.
      */
    override def isTextNode(node: N): (N, N) = (normalize(node.collect {
      case e@ChildStep(AnyTextNode, _) => e
      case e@DescendantStep(AnyTextNode, _) => e
    }), node)

    /** Predicate function that checks whether a node is a comment node.
      * The first result is a node that is known to be a comment node (this is BOTTOM if the node definitely
      * is not a comment node), the second result is a node that might not be a comment node (this is
      * BOTTOM if the node definitely is a comment node). The two results are not necessarily disjoint.
      */
    override def isComment(node: N): (N, N) = (normalize(node.collect {
      case e@ChildStep(AnyCommentNode, _) => e
      case e@DescendantStep(AnyCommentNode, _) => e
    }), node)

    /** Predicate function that checks whether a node is an attribute node.
      * The first result is a node that is known to be an attribute node (this is BOTTOM if the node definitely
      * is not an attribute node), the second result is a node that might not be an attribute node (this is
      * BOTTOM if the node definitely is an attribute node). The two results are not necessarily disjoint.
      */
    override def isAttribute(node: N): (N, N) = (normalize(node.collect {
      case e@ChildStep(NamedAttribute(_), _) => e
      case e@ChildStep(AnyAttribute, _) => e
      case e@DescendantStep(NamedAttribute(_), _) => e
      case e@DescendantStep(AnyAttribute, _) => e
    }), node)

    /** Predicate function that checks whether a node has a specified name.
      * The first result is a node that is known to have that name (this is BOTTOM if the node definitely
      * doesn't have that name), the second result is a node that might not have that name (this is
      * BOTTOM if the node definitely does have that name). The two results are not necessarily disjoint.
      * Nodes that don't have a name (any node except element and attribute nodes) are evaluated to BOTTOM.
      */
    override def hasName(node: N, name: String): (N, N) = (normalize(node.collect {
      case e@ChildStep(NamedElement(n), _) if name == n => List(e)
      case ChildStep(AnyElement, p) => List(ChildStep(NamedElement(name), p))
      case a@ChildStep(NamedAttribute(n), _) if name == n => List(a)
      case ChildStep(AnyAttribute, p) => List(ChildStep(NamedAttribute(name), p))
      case e@DescendantStep(NamedElement(n), _) if name == n => List(e)
      case DescendantStep(AnyElement, p) => List(DescendantStep(NamedElement(name), p))
      case a@DescendantStep(NamedAttribute(n), _) if name == n => List(a)
      case DescendantStep(AnyAttribute, p) => List(DescendantStep(NamedAttribute(name), p))
    }.flatten), node)

    /** Get the name for a given node. Nodes that don't have a name (i.e. are not an element or attribute node)
      * are evaluated to the empty string, not BOTTOM!
      */
    override def getNodeName(node: N): V = ???

    /** Concatenates the values of all text nodes in the list. List elements that are not text nodes are ignored. */
    override def getConcatenatedTextNodeValues(list: L): V = ???

    /** Filters a list using a given predicate function. The predicate function should never return a node
      * (as its first result) that is less precise than the input node.
      */
    override def filter(list: L, predicate: N => (N, N)): L = ???

    /** Gets the first node out of a node list. BOTTOM is returned if the list is empty or BOTTOM. */
    override def getFirst(list: L): N = ???
  }
}