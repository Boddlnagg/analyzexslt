package analysis.domain

import analysis.LatticeOrdering

/** An XML domain, providing operations on XML nodes (N) and list of nodes (L). */
trait XMLDomain[N, L, V] {
  // TODO: split this domain in two: XMLMatchingDomain with all operations needed by AbstractXPathMatcher
  // and XMLDomain extending that one with the remaining operations

  /** Get the TOP element for XML nodes. */
  def top: N

  /** Gets the BOTTOM element for XML nodes. */
  def bottom: N

  /** Get the TOP element for XML node lists.*/
  def topList: L // TODO: this is currently never used

  /** Gets the BOTTOM element for XML node lists. */
  def bottomList: L

  /** Join two nodes. This calculates their supremum (least upper bound). */
  def join(n1: N, n2: N): N

  /** Join a list of nodes. The supremum of the empty list is BOTTOM. */
  def join(nodes: List[N]): N = nodes match {
    case Nil => bottom
    case List(node) => node
    case _ => nodes.reduceLeft(join)
  }

  /** Join two node lists. This calculates their supremum (least upper bound). */
  def joinList(l1: L, l2: L): L

  /** Join a list of nodes lists. The supremum of the empty list is BOTTOM. */
  def joinList(lists: List[L]): L = lists match {
    case Nil => bottomList
    case List(list) => list
    case _ => lists.reduceLeft(joinList)
  }

  /** Compares two elements of the lattice of nodes.
    * TOP is always greater than everything else, BOTTOM is always less than everything else.
    */
  def compare(n1: N, n2: N): LatticeOrdering

  /** Compares two elements of the lattice of node lists.
    * TOP is always greater than everything else, BOTTOM is always less than everything else.
    */
  def compareList(l1: L, l2: L): LatticeOrdering // TODO: is this really needed?

  /** Create an element node with the given name, attributes and children.
    * The output is created bottom-up, so children are always created before their parent nodes.
    */
  def createElement(name: String, attributes: L, children: L): N // TODO: in order to support <xsl:element> this would need to take the name as V

  /** Create an element node with the given name and no children or attributes.
    * The output is created bottom-up, so children are always created before their parent nodes.
    */
  def createElement(name: String): N = createElement(name, createEmptyList(), createEmptyList()) // TODO: in order to support <xsl:element> this would need to take the name as V

  /** Create an attribute node with the given name and text value.
    * Values that are not strings evaluate to BOTTOM.
    */
  def createAttribute(name: String, value: V): N

  /** Create a text node with the given text value.
    * Values that are not strings evaluate to BOTTOM.
    */
  def createTextNode(value: V): N

  /** Create an emtpy list containing no nodes */
  def createEmptyList(): L

  /** Create a list containing a single abstract node */
  def createSingletonList(node: N): L

  /** Get the root node of a given node */
  def getRoot(node: N): N // TODO: this might be implementable using getParent() and isRoot()

  /** Get the list of attributes of a given node.
    * Nodes that are not an element (and therefore don't have attributes) return an empty list, not BOTTOM! */
  def getAttributes(node: N): L

  /** Get the list of children of a given node.
    * Root nodes have a single child, element nodes have an arbitrary number of children.
    * Nodes that don't have children return an empty list, not BOTTOM! */
  def getChildren(node: N): L

  /** Get the parent of given node. */
  def getParent(node: N): N

  /** Predicate function that checks whether a node has a specified node as its parent.
    * The first result is a node that is known to have that parent (this is BOTTOM if the node definitely
    * doesn't have that parent), the second result is a node that might not have that parent (this is
    * BOTTOM if the node definitely does have that parent). The two results are not necessarily disjoint.
    */
  def hasParent(node: N, parent: N): (N, N)

  /** Concatenates two lists. */
  def concatLists(list1: L, list2: L): L

  /** Partitions a node list in such a way that the first result contains all attribute nodes from the beginning of
    * the list (as soon as there are other node types in the list, attributes are ignored) and the second result
    * contains all other nodes.
    */
  def partitionAttributes(list: L): (L, L)

  /** Wraps a list of nodes in a document/root node. Lists that don't have exactly one element evaluate to BOTTOM. */
  def wrapInRoot(list: L): N

  /** Copies a list of nodes, so that they can be used in the output.
    * A root node is copied by copying its child (not wrapped in a root node). */
  def copyToOutput(list: L): L

  /** Evaluates a function for every element in the given list, providing also the index of each element in the list.
    * The resulting lists are flattened into a single list.
    */
  def flatMapWithIndex(list: L, f: (N, V) => L): L

  /** Gets the size of a node list */
  def getNodeListSize(list: L): V

  /** Gets the string-value of a node, as specified in the XSLT specification */
  def getStringValue(node: N): V

  /** Predicate function that checks whether a node is a root node.
    * The first result is a node that is known to be a root node (this is BOTTOM if the node definitely
    * is not a root node), the second result is a node that might not be a root node (this is
    * BOTTOM if the node definitely is a root node). The two results are not necessarily disjoint.
    */
  def isRoot(node: N): (N, N)

  /** Predicate function that checks whether a node is an element node.
    * The first result is a node that is known to be an element node (this is BOTTOM if the node definitely
    * is not an element node), the second result is a node that might not be an element node (this is
    * BOTTOM if the node definitely is an element node). The two results are not necessarily disjoint.
    */
  def isElement(node: N): (N, N)

  /** Predicate function that checks whether a node is a text node.
    * The first result is a node that is known to be a text node (this is BOTTOM if the node definitely
    * is not a text node), the second result is a node that might not be a text node (this is
    * BOTTOM if the node definitely is a text node). The two results are not necessarily disjoint.
    */
  def isTextNode(node: N): (N, N)

  /** Predicate function that checks whether a node is a comment node.
    * The first result is a node that is known to be a comment node (this is BOTTOM if the node definitely
    * is not a comment node), the second result is a node that might not be a comment node (this is
    * BOTTOM if the node definitely is a comment node). The two results are not necessarily disjoint.
    */
  def isComment(node: N): (N, N)

  /** Predicate function that checks whether a node is an attribute node.
    * The first result is a node that is known to be an attribute node (this is BOTTOM if the node definitely
    * is not an attribute node), the second result is a node that might not be an attribute node (this is
    * BOTTOM if the node definitely is an attribute node). The two results are not necessarily disjoint.
    */
  def isAttribute(node: N): (N, N)

  /** Predicate function that checks whether a node has a specified name.
    * The first result is a node that is known to have that name (this is BOTTOM if the node definitely
    * doesn't have that name), the second result is a node that might not have that name (this is
    * BOTTOM if the node definitely does have that name). The two results are not necessarily disjoint.
    * Nodes that don't have a name (any node except element and attribute nodes) are evaluated to BOTTOM.
    */
  def hasName(node: N, name: String): (N, N)

  /** Get the name for a given node. Nodes that don't have a name (i.e. are not an element or attribute node)
    * are evaluated to the empty string, not BOTTOM!
    */
  def getNodeName(node: N): V

  /** Concatenates the values of all text nodes in the list. List elements that are not text nodes are ignored. */
  def getConcatenatedTextNodeValues(list: L): V

  /** Filters a list using a given predicate function. The predicate function should never return a node
  * (as its first result) that is less precise than the input node.
  */
  def filter(list: L, predicate: N => (N, N)): L

  /** Get a list of all descendants of a node. */
  def getDescendants(node: N): L = {
    val children = getChildren(node)
    flatMapWithIndex(children, {
      case (n, _) => concatLists(createSingletonList(n), getDescendants(n))
    })
  }
}