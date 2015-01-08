package analysis.domain.concrete

import analysis.domain.{XPathDomain, XMLDomain}
import xml._

/** Just a wrapper for the type alias */
object ConcreteXMLDomain {
  type N = SingleValueLattice[XMLNode]
  type L = SingleValueLattice[List[XMLNode]]

  /** This is the actual (partial) domain implementation */
  trait D[V] extends XMLDomain[N, L, V] {
    val xpathDom: XPathDomain[V, N, L]

    /** Get the TOP element for XML nodes. */
    override def top: N = Top

    /** Gets the BOTTOM element for XML nodes. */
    override def bottom: N = Bottom

    /** Get the TOP element for XML node lists.*/
    override def topList: L = Top

    /** Gets the BOTTOM element for XML node lists. */
    override def bottomList: L = Bottom

    /** Calcucate the join of two abstract nodes. This is the supremum (least upper bound). */
    override def join(n1: N, n2: N): N = n1.join(n2)

    /** Calculate the meet of two abstract nodes. This is the infimum (greatest lower bound). */
    override def meet(n1: N, n2: N): N = n1.meet(n2)

    /** Join two node lists. This calculates their supremum (least upper bound). */
    override def joinLists(l1: L, l2: L): L = l1.join(l2)

    /** Compares two elements of the lattice of nodes.
      * Returns true if n1 < n2 or n1 = n2, false if n1 > n2 or if they are incomparable.
      */
    override def lessThanOrEqual(n1: N, n2: N): Boolean = n1.lessThanOrEqual(n2)

    /** Compares two elements of the lattice of node lists.
      * Returns true if l1 < l2 or l1 = l2, false if l1 > l2 or if they are incomparable.
      */
    override def lessThanOrEqualLists(l1: L, l2: L): Boolean = l1.lessThanOrEqual(l2)

    /** Create an element node with the given name, attributes and children.
      * The output is created bottom-up, so children are always created before their parent nodes.
      * Consecutive text node children must be merged into a single text node by this method.
      */
    override def createElement(name: String, attributes: L, children: L): N = (attributes, children) match {
      case (Value(attr), Value(chld)) => try Value(XMLElement(name, attr.map(_.asInstanceOf[XMLAttribute]), chld))
        catch {
          case e: ClassCastException => Bottom
          case e: IllegalArgumentException => Bottom
        }
      case (Bottom, _) => Bottom
      case (_, Bottom) => Bottom
      case _ => Top
    }

    /** Create an emtpy list containing no nodes */
    override def createEmptyList(): L = Value(Nil)

    /** Create a list containing a single abstract node */
    override def createSingletonList(node: N): L = node.map(n => List(n))

    /** Get the root node of a given node */
    override def getRoot(node: N): N = node.map(_.root)

    /** Get the list of attributes of a given node.
      * Nodes that are not an element (and therefore don't have attributes) return an empty list, not BOTTOM! */
    override def getAttributes(node: N): L = node.map {
      case XMLElement(_, attr, _, _) => attr.toList
      case _ => Nil
    }

    /** Get the list of children of a given node.
      * Root nodes have a single child, element nodes have an arbitrary number of children.
      * Nodes that don't have children return an empty list, not BOTTOM! */
    override def getChildren(node: N): L = node.map {
      case XMLElement(_, _, children, _) => children.toList
      case XMLRoot(inner) => List(inner)
      case _ => Nil
    }

    /** Get the parent of given node. If the node has no parent (root node), BOTTOM is returned. */
    override def getParent(node: N): N = node.map {
      case XMLRoot(_) => return Bottom
      case e => e.parent
    }

    /** Predicate function that checks whether a node is in a given list of nodes.
      * The first result is a node that is known to be in that list (this is BOTTOM if the node definitely
      * is not in the list), the second result is a node that might not be in the list (this is
      * BOTTOM if the node definitely is contained in the list). The two results are not necessarily disjoint.
      */
    override def isContainedIn(node: N, list: L): (N, N) = (node, list) match {
      case (Bottom, _) => (Bottom, Bottom)
      case (_, Bottom) => (Bottom, node) // list is BOTTOM -> can't be contained in it
      case (Top, _) => (Top, Top) // don't know anything about the node
      case (Value(_), Top) => (node, node) //  list is TOP -> don't know if it contains the node
      case (Value(n), Value(l)) => if (l.contains(n)) (node, Bottom) else (Bottom, node)
    }

    /** Concatenates two lists. */
    override def concatLists(list1: L, list2: L): L = list1.liftBinaryOp(list2) {
      (l1, l2) => l1 ++ l2
    }

    /** Partitions a node list in such a way that the first result contains all attribute nodes from the beginning of
      * the list (as soon as there are other node types in the list, attributes are ignored) and the second result
      * contains all other nodes.
      */
    override def partitionAttributes(list: L): (L, L) = list match {
      case Top => (Top, Top)
      case Bottom => (Bottom, Bottom)
      case Value(l) =>
        val resultAttributes = l.takeWhile(n => n.isInstanceOf[XMLAttribute]) // NOTE: takeWhile, not filter!
        val resultChildren = l.filter(n => !n.isInstanceOf[XMLAttribute])
        (Value(resultAttributes), Value(resultChildren))
    }

    /** Wraps a list of nodes in a document/root node. Lists that don't have exactly one element evaluate to BOTTOM. */
    override def wrapInRoot(list: L): N = list match {
      case Top => Top
      case Value(List(inner: XMLElement)) => Value(XMLRoot(inner))
      case _ => Bottom
    }

    /** Copies a list of nodes, so that they can be used in the output.
      * A root node is copied by copying its child (not wrapped in a root node). */
    override def copyToOutput(list: L): L = list.map(_.map({
      case XMLRoot(inner) => inner.copy // "a root node is copied by copying its children" according to spec
      case node => node.copy
    }))

    /** Evaluates a function for every element in the given list, providing also the index of each element in the list.
      * The resulting lists are flattened into a single list.
      */
    override def flatMapWithIndex(list: L, f: (N, V) => L): L = list match {
      case Top => Top
      case Value(l) =>
        val mapped: List[L] = l.zipWithIndex.map {
          case (n, i) => f(Value(n), xpathDom.liftNumber(i))
        }
        if (mapped.contains(Bottom)) Bottom
        else if (mapped.contains(Top)) Top
        else {
          val unpacked = mapped.map {
            case Value(v) => v
          }
          Value(unpacked.flatten)
        }
      case Bottom => Bottom
    }

    /** Gets the size of a node list */
    override def getNodeListSize(list: L): V = list match {
      case Top => xpathDom.top
      case Bottom => xpathDom.bottom
      case Value(l) => xpathDom.liftNumber(l.size)
    }

    /** Gets the string-value of a node, as specified in the XSLT specification */
    override def getStringValue(node: N): V = node match {
      case Top => xpathDom.top
      case Bottom => xpathDom.bottom
      case Value(n) => xpathDom.liftString(n.stringValue)
    }

    /** Predicate function that checks whether a node is a root node.
      * The first result is a node that is known to be a root node (this is BOTTOM if the node definitely
      * is not a root node), the second result is a node that might not be a root node (this is
      * BOTTOM if the node definitely is a root node). The two results are not necessarily disjoint.
      */
    override def isRoot(node: N): (N, N) = node match {
      case Bottom => (Bottom, Bottom)
      case Top => (Top, Top)
      case Value(n) => n match {
        case XMLRoot(_) => (node, Bottom)
        case _ => (Bottom, node)
      }
    }

    /** Predicate function that checks whether a node is an element node.
      * The first result is a node that is known to be an element node (this is BOTTOM if the node definitely
      * is not an element node), the second result is a node that might not be an element node (this is
      * BOTTOM if the node definitely is an element node). The two results are not necessarily disjoint.
      */
    override def isElement(node: N): (N, N) = node match {
      case Bottom => (Bottom, Bottom)
      case Top => (Top, Top)
      case Value(n) => n match {
        case XMLElement(_, _, _, _) => (node, Bottom)
        case _ => (Bottom, node)
      }
    }

    /** Predicate function that checks whether a node is a text node.
      * The first result is a node that is known to be a text node (this is BOTTOM if the node definitely
      * is not a text node), the second result is a node that might not be a text node (this is
      * BOTTOM if the node definitely is a text node). The two results are not necessarily disjoint.
      */
    override def isTextNode(node: N): (N, N) = node match {
      case Bottom => (Bottom, Bottom)
      case Top => (Top, Top)
      case Value(n) => n match {
        case XMLTextNode(_, _) => (node, Bottom)
        case _ => (Bottom, node)
      }
    }

    /** Predicate function that checks whether a node is a comment node.
      * The first result is a node that is known to be a comment node (this is BOTTOM if the node definitely
      * is not a comment node), the second result is a node that might not be a comment node (this is
      * BOTTOM if the node definitely is a comment node). The two results are not necessarily disjoint.
      */
    override def isComment(node: N): (N, N) = node match {
      case Bottom => (Bottom, Bottom)
      case Top => (Top, Top)
      case Value(n) => n match {
        case XMLComment(_, _) => (node, Bottom)
        case _ => (Bottom, node)
      }
    }

    /** Predicate function that checks whether a node is an attribute node.
      * The first result is a node that is known to be an attribute node (this is BOTTOM if the node definitely
      * is not an attribute node), the second result is a node that might not be an attribute node (this is
      * BOTTOM if the node definitely is an attribute node). The two results are not necessarily disjoint.
      */
    override def isAttribute(node: N): (N, N) = node match {
      case Bottom => (Bottom, Bottom)
      case Top => (Top, Top)
      case Value(n) => n match {
        case XMLAttribute(_, _, _) => (node, Bottom)
        case _ => (Bottom, node)
      }
    }

    /** Predicate function that checks whether a node has a specified name.
      * The first result is a node that is known to have that name (this is BOTTOM if the node definitely
      * doesn't have that name), the second result is a node that might not have that name (this is
      * BOTTOM if the node definitely does have that name). The two results are not necessarily disjoint.
      * Nodes that don't have a name (any node except element and attribute nodes) are evaluated to BOTTOM.
      */
    override def hasName(node: N, name: String): (N, N) = node match {
      case Bottom => (Bottom, Bottom)
      case Top => (Top, Top)
      case Value(n) => n match {
        case XMLElement(elemname, _, _, _) if elemname == name => (node, Bottom)
        case XMLAttribute(attribname, _, _) if attribname == name => (node, Bottom)
        case _ => (Bottom, node)
      }
    }

    /** Get the name for a given node. Nodes that don't have a name (i.e. are not an element or attribute node)
      * are evaluated to the empty string, not BOTTOM!
      */
    override def getNodeName(node: N): V = node match {
      case Top => xpathDom.top
      case Bottom => xpathDom.bottom
      case Value(n) => xpathDom.liftString(n match {
        case XMLElement(name, _, _, _) => name
        case XMLAttribute(name, _, _) => name
        case _ => ""
      })
    }

    /** Concatenates the values of all text nodes in the list. List elements that are not text nodes are ignored. */
    override def getConcatenatedTextNodeValues(list: L): V = list match {
      case Top => xpathDom.top
      case Bottom => xpathDom.bottom
      case Value(l) => xpathDom.liftString(l.collect { case XMLTextNode(text, _) => text }.mkString(""))
    }

    /** Filters a list using a given predicate function. The predicate function should never return a node
      * (as its first result) that is less precise than the input node.
      */
    override def filter(list: L, predicate: N => (N, N)): L = list match {
      case Top => Top
      case Bottom => Bottom
      case Value(l) => Value(l.filter { node =>
        val (predicateTrue, _) = predicate(Value(node))
        predicateTrue match {
          case Bottom => false
          case Value(n) if n == node => true
          case _  => throw new AssertionError("predicate should never return Top or a different element from what it was given")
        }
      })
    }

    /** Gets the first node out of a node list. BOTTOM is returned if the list is empty or BOTTOM. */
    override def getFirst(list: L): N = list match {
      case Top => Top // we don't know the first node
      case Bottom => Bottom // there is no first node
      case Value(l) => l match {
        case Nil => Bottom // there is no first node because the list is empty
        case first :: _ => Value(first) // there is a first node and we know what it is
      }
    }
  }
}

