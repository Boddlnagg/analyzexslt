package analysis.domain.powerset

import analysis.domain.{Lattice, XPathDomain, XMLDomain}
import xml._

/** Just a wrapper for the type aliases */
object PowersetXMLDomain {
  type N = Option[Set[XMLNode]] // None represents the infinite set, Some represents finite sets
  type L = Option[Set[List[XMLNode]]] // None represents the infinite set, Some represents finite sets

  /** This is the actual (partial) domain implementation */
  trait D[V] extends XMLDomain[N, L, V] {
    val xpathDom: XPathDomain[V, N, L]

    private val latN = Lattice.createFromOptionalSet[XMLNode]
    private val latL = Lattice.createFromOptionalSet[List[XMLNode]]

    /** Get the TOP element for XML nodes. */
    override def top: N = latN.top

    /** Gets the BOTTOM element for XML nodes. */
    override def bottom: N = BOT

    protected val BOT: N = latN.bottom

    /** Get the TOP element for XML node lists.*/
    override def topList: L = latL.top

    /** Gets the BOTTOM element for XML node lists. */
    override def bottomList: L = BOT_LIST

    protected val BOT_LIST: L = latL.bottom

    /** Calculate the join of two abstract nodes. This is the supremum (least upper bound). */
    override def join(n1: N, n2: N): N = latN.join(n1, n2)

    /** Calculate the meet of two abstract nodes. This is the infimum (greatest lower bound). */
    override def meet(n1: N, n2: N): N = latN.meet(n1, n2)

    /** Compares two elements of the lattice of nodes.
      * Returns true if n1 < n2 or n1 = n2, false if n1 > n2 or if they are incomparable.
      */
    override def lessThanOrEqual(n1: N, n2: N): Boolean = latN.lessThanOrEqual(n1, n2)

    /** Compares two elements of the lattice of node lists.
      * Returns true if l1 < l2 or l1 = l2, false if l1 > l2 or if they are incomparable.
      */
    override def lessThanOrEqualLists(l1: L, l2: L): Boolean = latL.lessThanOrEqual(l1, l2)

    /** Join two node lists. This calculates their supremum (least upper bound). */
    override def joinLists(l1: L, l2: L): L = latL.join(l1, l2)

    /** Create an emtpy list containing no nodes */
    override def createEmptyList(): L = Some(Set(Nil))

    /** Create a list containing a single abstract node */
    override def createSingletonList(node: N): L = node.map(s => s.map(n => List(n)))

    /** Get the root node of a given node */
    override def getRoot(node: N): N = node.map(_.map(n => n.root))

    /** Get the list of attributes of a given node.
      * Nodes that are not an element (and therefore don't have attributes) return an empty list, not BOTTOM!
      */
    override def getAttributes(node: N): L = node.map(_.map {
      case XMLElement(_, attr, _, _) => attr.toList
      case _ => Nil // NOTE: other node types have no attributes, but this must NOT evaluate to BOTTOM
    })

    /** Get the list of children of a given node.
      * Root nodes have a single child, element nodes have an arbitrary number of children.
      * Nodes that don't have children return an empty list, not BOTTOM!
      */
    override def getChildren(node: N): L = node.map(_.map{
      case XMLRoot(children) => children
      case XMLElement(_, _, children, _) => children.toList
      case _ => Nil // NOTE: other node types have no children, but this must NOT evaluate to BOTTOM
    })

    /** Get the parent of given node. If the node has no parent (root node), BOTTOM is returned. */
    override def getParent(node: N): N = node.map(_.collect {
      case e if !e.isInstanceOf[XMLRoot] =>
        assert(e.parent != null)
        e.parent
    })

    /** Concatenates two lists. */
    override def concatLists(list1: L, list2: L): L = (list1, list2) match {
      case (BOT_LIST, _) => BOT_LIST
      case (_, BOT_LIST) => BOT_LIST
      case (Some(l1), Some(l2)) => Some(l1.cross(l2).map {
        case (ll1, ll2) => ll1 ++ ll2
      }.toSet)
      case _ => None // at least one operand is TOP and the other is not BOTTOM
    }

    /** Creates a root node with the given children. The second parameter specifies whether the root represents a
      * (result tree) fragment or a complete document (the latter can only have a single element child).
      */
    override def createRoot(children: L, isResultTreeFragment: Boolean): N = children.map(_.collect {
      case ch if isResultTreeFragment => XMLRoot(ch)
      case ch@List(single: XMLElement) => XMLRoot(ch)
      // NOTE: when isResultTreeFragment == false, lists with more than a single element node are implicitly ignored
    })

    /** Copies a list of nodes, so that they can be used in the output.
      * A root node is copied by copying its child (not wrapped in a root node).
      */
    override def copyToOutput(list: L): L = list.map(_.map(_.flatMap {
      case XMLRoot(children) => children.map(_.copy) // "a root node is copied by copying its children" according to spec
      case node => List(node.copy)
    }))

    /** Gets the size of a node list */
    override def getNodeListSize(list: L): V = list match {
      case None => xpathDom.topNumber
      case Some(s) => xpathDom.joinAll(s.map(l => xpathDom.liftNumber(l.size)))
    }

    /** Gets the string-value of a node, as specified in the XSLT specification */
    override def getStringValue(node: N): V = node match {
      case None => xpathDom.topString
      case Some(s) => xpathDom.joinAll(s.map(n => xpathDom.liftString(n.stringValue)))
    }

    /** Predicate function that checks whether a node is a root node.
      * The first result is a node that is known to be a root node (this is BOTTOM if the node definitely
      * is not a root node), the second result is a node that might not be a root node (this is
      * BOTTOM if the node definitely is a root node). The two results are not necessarily disjoint.
      */
    override def isRoot(node: N, allowResultTreeFragments: Boolean): (N, N) = node match {
      case None => (None, None)
      case Some(s) =>
        val (yes, no) =
          if (allowResultTreeFragments)
            s.partition(_.isInstanceOf[XMLRoot])
          else
            s.partition { n =>
              n.isInstanceOf[XMLRoot] && (n.asInstanceOf[XMLRoot].children match {
                case List(single: XMLElement) => true
                case _ => false
              })
            }
        (Some(yes), Some(no))
    }

    /** Predicate function that checks whether a node is an element node.
      * The first result is a node that is known to be an element node (this is BOTTOM if the node definitely
      * is not an element node), the second result is a node that might not be an element node (this is
      * BOTTOM if the node definitely is an element node). The two results are not necessarily disjoint.
      */
    override def isElement(node: N): (N, N) = node match {
      case None => (None, None)
      case Some(s) =>
        val (yes, no) = s.partition(_.isInstanceOf[XMLElement])
        (Some(yes), Some(no))
    }

    /** Predicate function that checks whether a node is a text node.
      * The first result is a node that is known to be a text node (this is BOTTOM if the node definitely
      * is not a text node), the second result is a node that might not be a text node (this is
      * BOTTOM if the node definitely is a text node). The two results are not necessarily disjoint.
      */
    override def isTextNode(node: N): (N, N) = node match {
      case None => (None, None)
      case Some(s) =>
        val (yes, no) = s.partition(_.isInstanceOf[XMLTextNode])
        (Some(yes), Some(no))
    }

    /** Predicate function that checks whether a node is a comment node.
      * The first result is a node that is known to be a comment node (this is BOTTOM if the node definitely
      * is not a comment node), the second result is a node that might not be a comment node (this is
      * BOTTOM if the node definitely is a comment node). The two results are not necessarily disjoint.
      */
    override def isComment(node: N): (N, N) = node match {
      case None => (None, None)
      case Some(s) =>
        val (yes, no) = s.partition(_.isInstanceOf[XMLComment])
        (Some(yes), Some(no))
    }

    /** Predicate function that checks whether a node is an attribute node.
      * The first result is a node that is known to be an attribute node (this is BOTTOM if the node definitely
      * is not an attribute node), the second result is a node that might not be an attribute node (this is
      * BOTTOM if the node definitely is an attribute node). The two results are not necessarily disjoint.
      */
    override def isAttribute(node: N): (N, N) = node match {
      case None => (None, None)
      case Some(s) =>
        val (yes, no) = s.partition(_.isInstanceOf[XMLAttribute])
        (Some(yes), Some(no))
    }

    /** Predicate function that checks whether a node has a specified name.
      * The first result is a node that is known to have that name (this is BOTTOM if the node definitely
      * doesn't have that name), the second result is a node that might not have that name (this is
      * BOTTOM if the node definitely does have that name). The two results are not necessarily disjoint.
      * Nodes that don't have a name (any node except element and attribute nodes) are evaluated to BOTTOM.
      */
    override def hasName(node: N, name: String): (N, N) = node match {
      case None => (None, None)
      case Some(s) =>
        // nodes that can't have a name are evaluated to bottom implicitly, i.e. they won't appear in the output at all
        val yes = s.filter {
          case XMLElement(elementName, _, _, _) => elementName == name
          case XMLAttribute(attributeName, _, _) => attributeName == name
          case _ => false
        }
        val no = s.filter {
        case XMLElement(elementName, _, _, _) => elementName != name
        case XMLAttribute(attributeName, _, _) => attributeName != name
        case _ => false
        }
        (Some(yes), Some(no))
    }

    /** Predicate function that checks whether a node is in a given list of nodes.
      * The first result is a node that is known to be in that list (this is BOTTOM if the node definitely
      * is not in the list), the second result is a node that might not be in the list (this is
      * BOTTOM if the node definitely is contained in the list). The two results are not necessarily disjoint.
      */
    override def isContainedIn(node: N, list: L): (N, N) = (node, list) match {
      case (BOT, _) => (BOT, BOT)
      case (_, BOT_LIST) => (BOT, node) // list is BOTTOM -> can't be contained in it
      case (None, _) => (None, None) // don't know anything about the node
      case (Some(_), None) => (node, node) //  list is TOP (or has unknown elements) -> don't know if it contains the node
      case (Some(nodes), Some(lists)) =>
        val (yes, no) = nodes.partition(n => lists.exists(l => l.contains(n)))
        (Some(yes), Some(no))
    }

    /** Get the name for a given node. Nodes that don't have a name (i.e. are not an element or attribute node)
      * are evaluated to the empty string, not BOTTOM!
      */
    override def getNodeName(node: N): V = node match {
      case None => xpathDom.topString
      case Some(s) => xpathDom.joinAll(s.map {
        case XMLElement(nodeName, _, _, _) => xpathDom.liftString(nodeName)
        case XMLAttribute(nodeName, _, _) => xpathDom.liftString(nodeName)
        case _ => xpathDom.liftString("")
      })
    }

    /** Concatenates the values of all text nodes in the list. List elements that are not text nodes are ignored. */
    override def getConcatenatedTextNodeValues(list: L): V = list match {
      case None => xpathDom.topString
      case Some(s) => xpathDom.joinAll(s.map { l =>
        xpathDom.liftString(l.collect { case n: XMLTextNode => n.value }.mkString)
      })
    }

    /** Filters a list using a given predicate function. The predicate function should never return a node
      * (as its first result) that is less precise than the input node.
      */
    override def filter(list: L, predicate: N => (N, N)): L = list.map(_.map(_.filter { n =>
      val node: N = Some(Set(n))
      val (resultTrue, _) = predicate(node)
      assert(lessThanOrEqual(resultTrue, node))
      resultTrue.get.toList match {
        case Nil => false // list without elements -> element was filtered out
        case first :: Nil => true // list with one element -> element was not filtered out
        case _ =>
          // list with more than one element -> this should not happen in this domain
          throw new AssertionError("Filter predicate returned node with more than one possibility.")
      }
    }))

    /** Takes the longest prefix of a list where all elements fulfill a given predicate function.
      * The predicate function should never return a node (as its first result) that is less precise than the input node.
      */
    override def takeWhile(list: L, predicate: N => (N, N)): L = list.map(_.map(_.takeWhile { n =>
      val node: N = Some(Set(n))
      val (resultTrue, _) = predicate(node)
      assert(lessThanOrEqual(resultTrue, node))
      resultTrue.get.toList match {
        case Nil => false // list without elements -> element was filtered out
        case first :: Nil => true // list with one element -> element was not filtered out
        case _ =>
          // list with more than one element -> this should not happen in this domain
          throw new AssertionError("Predicate for takeWhile returned node with more than one possibility.")
      }
    }))

    /** Evaluates a function for every element in the given list, providing also the index of each element in the list.
      * The resulting lists are flattened into a single list by concatenation.
      */
    override def flatMapWithIndex(list: L, f: (N, V) => L): L = list match {
      case None => // list with unknown number of unknown elements
        val mappedNone = f(None, xpathDom.topNumber) // call f with TOP element and TOP index
        mappedNone match {
          case Some(s) => s.toList match {
            case Nil => BOT_LIST // f always returns BOTTOM -> return BOTTOM
            case List(Nil) => Some(Set(Nil)) // f always returns an empty list -> return empty list
            case _ => None // f returns some elements
          }
          case None => None // don't result elements -> don't know output
        }
      case Some(s) => s.map { l =>
        val mapped = l.zipWithIndex.map { case (n, i) => f(Some(Set(n)), xpathDom.liftNumber(i)) }
        val flattened = mapped.foldLeft(createEmptyList())((acc, next) => concatLists(acc, next))
        flattened
      }.fold(bottomList)(joinLists)
    }

    /** Gets the first node out of a node list. BOTTOM is returned if the list is empty or BOTTOM. */
    override def getFirst(list: L): N = list.map(_.collect {
      case head :: _ => head
    })
  }
}
