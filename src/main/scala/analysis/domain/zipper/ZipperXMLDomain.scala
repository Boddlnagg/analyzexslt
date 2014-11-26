package analysis.domain.zipper

import analysis.domain.{XPathDomain, XMLDomain, Lattice}

/** Just a wrapper for the type aliases */
object ZipperXMLDomain {
  type S = ZipperTree // type of subtrees
  type P = Set[Path] // type of paths
  type N = (S, P) // a node is a subtree and a path
  type L = ZList[N]

  abstract class NodeDescriptor
  case object RootNode extends NodeDescriptor
  case class ElementNode(name: String) extends NodeDescriptor
  case class AttributeNode(name: String, value: String) extends NodeDescriptor
  case class TextNode(value: String) extends NodeDescriptor
  case class CommentNode(value: String) extends NodeDescriptor

  case class ZipperTree(desc: Option[Set[NodeDescriptor]], children: ZList[ZipperTree])

  implicit object ZipperTreeLattice extends Lattice[ZipperTree] {

    def top = topTree
    def bottom = ZipperTree(Some(Set()), ZBottom())
    def join(left: ZipperTree, right: ZipperTree): ZipperTree = ZipperTree(latD.join(left.desc, right.desc), left.children | right.children)
    def meet(left: ZipperTree, right: ZipperTree): ZipperTree = ZipperTree(latD.meet(left.desc, right.desc), left.children & right.children)
    def lessThanOrEqual(left: ZipperTree, right: ZipperTree): Boolean = latD.lessThanOrEqual(left.desc, right.desc) && left.children <= right.children
  }

  private val latS: Lattice[S] = ZipperTreeLattice // lattice for subtrees
  private val latP = Path.PathSetLattice // lattice for paths
  private val latD = Lattice.createFromOptionalSet[NodeDescriptor] // lattice for descriptors

  implicit object NodeLattice extends Lattice[N] {
    def top = (latS.top, latP.top)
    def bottom = (latS.bottom, latP.bottom)
    def join(left: N, right: N): N = normalize(latS.join(left._1, right._1), latP.join(left._2, right._2)) // TODO: normalize is probably not required here
    def meet(left: N, right: N): N = normalize(latS.meet(left._1, right._1), latP.meet(left._2, right._2))

    def lessThanOrEqual(left: N, right: N): Boolean =
      latS.lessThanOrEqual(left._1, right._1) && latP.lessThanOrEqual(left._2, right._2)
  }

  private def getDescriptorsFromPaths(path: P): Option[Set[NodeDescriptor]] = {
    def getSingle(desc: PatternStepDescriptor): Option[NodeDescriptor] = desc match {
        // TODO: support more cases in NodeDescriptor
      case AnyElement => None // TOP
      case NamedElement(name) => Some(ElementNode(name))
      case AnyAttribute => None
      case NamedAttribute(name) => None
      case AnyTextNode => None
      case AnyCommentNode => None
    }

    Some(path.map {
      case RootPath => RootNode
      case ChildStep(desc, _) => getSingle(desc) match {
        case Some(res) => res
        case None => return None
      }
      case DescendantStep(desc, _) => getSingle(desc) match {
        case Some(res) => res
        case None => return None
      }
    })
  }

  private def getPathsFromDescriptors(descriptors: Option[Set[NodeDescriptor]]): P = descriptors match {
    case None => latP.top
    case Some(s) => s.map {
      case RootNode => RootPath
      case ElementNode(name) => DescendantStep(NamedElement(name), RootPath)
      case AttributeNode(name, _) => DescendantStep(NamedAttribute(name), RootPath)
      case TextNode(_) => DescendantStep(AnyTextNode, RootPath)
      case CommentNode(_) => DescendantStep(AnyCommentNode, RootPath)
    }
  }

  /** Removes impossible elements (where Path and Subtree descriptors don't agree) */
  private def normalize(node: N) = {
    val (ZipperTree(desc, children), path) = node
    (ZipperTree(latD.meet(getDescriptorsFromPaths(path), desc), children), latP.meet(getPathsFromDescriptors(desc), path))
  }

  val topTree = latS.top

  /** This is the actual (partial) domain implementation */
  trait D[V] extends XMLDomain[N, L, V] {
    val xpathDom: XPathDomain[V, N, L]

    /** Get the TOP element for XML nodes. */
    override def top: N = NodeLattice.top

    /** Gets the BOTTOM element for XML nodes. */
    override def bottom: N = NodeLattice.bottom

    /** Get the TOP element for XML node lists.*/
    override def topList: L = ZTop()
    // TODO: this is currently never used

    /** Gets the BOTTOM element for XML node lists. */
    override def bottomList: L = ZBottom()

    /** Calcucate the join of two abstract nodes. This is the supremum (least upper bound). */
    override def join(n1: N, n2: N): N = NodeLattice.join(n1, n2)

    /** Calculate the meet of two abstract nodes. This is the infimum (greatest lower bound). */
    override def meet(n1: N, n2: N): N = NodeLattice.meet(n1, n2)

    /** Join two node lists. This calculates their supremum (least upper bound). */
    override def joinList(l1: L, l2: L): L = l1 | l2

    /** Compares two elements of the lattice of nodes.
      * Returns true if n1 < n2 or n1 = n2, false if n1 > n2 or if they are incomparable.
      */
    override def lessThanOrEqual(n1: N, n2: N): Boolean = NodeLattice.lessThanOrEqual(n1, n2)
    
    /** Compares two elements of the lattice of node lists.
      * Returns true if l1 < l2 or l1 = l2, false if l1 > l2 or if they are incomparable.
      */
    override def lessThanOrEqualList(l1: L, l2: L): Boolean = l1 <= l2
    // TODO: is this operation really needed (could be replaced with isBottom)?

    /** Create an element node with the given name, attributes and children.
      * The output is created bottom-up, so children are always created before their parent nodes.
      */
    override def createElement(name: String, attributes: L, children: L): N = {
      val tree = ZipperTree(Some(Set(ElementNode(name))), attributes.map(_._1) ++ children.map(_._1))
      val path = Set[Path]() // TODO: use RootPath here?
      (tree, path)
    }
    
    /** Create an attribute node with the given name and text value.
      * Values that are not strings evaluate to BOTTOM.
      */
    override def createAttribute(name: String, value: V): N = ???

    /** Create a text node with the given text value.
      * Values that are not strings evaluate to BOTTOM.
      */
    override def createTextNode(value: V): N = ???

    /** Create an emtpy list containing no nodes */
    override def createEmptyList(): L = ZNil()

    /** Create a list containing a single abstract node */
    override def createSingletonList(node: N): L = ZCons(node, ZNil())

    /** Get the root node of a given node */
    override def getRoot(node: N): N = {
      val (ZipperTree(desc, children), path) = node
      val root: P = Set(RootPath)
      normalize(ZipperTree(latD.meet(desc, getDescriptorsFromPaths(root)), children), latP.meet(path, root))
    }
    // TODO: this might be implementable using getParent() and isRoot()

    /** Get the list of attributes of a given node.
      * Nodes that are not an element (and therefore don't have attributes) return an empty list, not BOTTOM! */
    override def getAttributes(node: N): L = ???

    /** Get the list of children of a given node.
      * Root nodes have a single child, element nodes have an arbitrary number of children.
      * Nodes that don't have children return an empty list, not BOTTOM! */
    override def getChildren(node: N): L = ???

    /** Get the parent of given node. If the node has no parent (root node), BOTTOM is returned. */
    override def getParent(node: N): N = {
      val (ZipperTree(desc, children), path) = node
      val parent = latP.getParent(path)
      val newChildren: ZList[ZipperTree] = ZTop() // don't know anything about siblings of `node`
      normalize(ZipperTree(getDescriptorsFromPaths(parent), newChildren), parent)
    }

    /** Predicate function that checks whether a node is in a given list of nodes.
      * The first result is a node that is known to be in that list (this is BOTTOM if the node definitely
      * is not in the list), the second result is a node that might not be in the list (this is
      * BOTTOM if the node definitely is contained in the list). The two results are not necessarily disjoint.
      */
    override def isContainedIn(node: N, list: L): (N, N) = (list.contains(node), node)

    /** Concatenates two lists. */
    override def concatLists(list1: L, list2: L): L = list1 ++ list2

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
    override def getNodeListSize(list: L): V = list match {
      case ZBottom() => xpathDom.bottom
      case ZTop() => xpathDom.topNumber
      case ZUnknownLength(elems) => xpathDom.topNumber
      case ZCons(head, tail) => xpathDom.add(xpathDom.liftNumber(1), getNodeListSize(tail))
      case ZMaybeNil(head, tail) => xpathDom.join(xpathDom.liftNumber(0), xpathDom.add(xpathDom.liftNumber(1), getNodeListSize(tail)))
      case ZNil() => xpathDom.liftNumber(0)
    }

    /** Gets the string-value of a node, as specified in the XSLT specification */
    override def getStringValue(node: N): V = ???

    /** Predicate function that checks whether a node is a root node.
      * The first result is a node that is known to be a root node (this is BOTTOM if the node definitely
      * is not a root node), the second result is a node that might not be a root node (this is
      * BOTTOM if the node definitely is a root node). The two results are not necessarily disjoint.
      */
    override def isRoot(node: N): (N, N) = {
      val (ZipperTree(desc, children), path) = node
      // TODO: this might be problematic because we don't gain any information about the children
      val positiveResult: N = normalize(ZipperTree(latD.meet(desc, Some(Set(RootNode))), children), latP.meet(path, Set(RootPath)))
      (positiveResult, node)
    }

    /** Predicate function that checks whether a node is an element node.
      * The first result is a node that is known to be an element node (this is BOTTOM if the node definitely
      * is not an element node), the second result is a node that might not be an element node (this is
      * BOTTOM if the node definitely is an element node). The two results are not necessarily disjoint.
      */
    override def isElement(node: N): (N, N) = ???

    /** Predicate function that checks whether a node is a text node.
      * The first result is a node that is known to be a text node (this is BOTTOM if the node definitely
      * is not a text node), the second result is a node that might not be a text node (this is
      * BOTTOM if the node definitely is a text node). The two results are not necessarily disjoint.
      */
    override def isTextNode(node: N): (N, N) = ???

    /** Predicate function that checks whether a node is a comment node.
      * The first result is a node that is known to be a comment node (this is BOTTOM if the node definitely
      * is not a comment node), the second result is a node that might not be a comment node (this is
      * BOTTOM if the node definitely is a comment node). The two results are not necessarily disjoint.
      */
    override def isComment(node: N): (N, N) = ???

    /** Predicate function that checks whether a node is an attribute node.
      * The first result is a node that is known to be an attribute node (this is BOTTOM if the node definitely
      * is not an attribute node), the second result is a node that might not be an attribute node (this is
      * BOTTOM if the node definitely is an attribute node). The two results are not necessarily disjoint.
      */
    override def isAttribute(node: N): (N, N) = ???

    /** Predicate function that checks whether a node has a specified name.
      * The first result is a node that is known to have that name (this is BOTTOM if the node definitely
      * doesn't have that name), the second result is a node that might not have that name (this is
      * BOTTOM if the node definitely does have that name). The two results are not necessarily disjoint.
      * Nodes that don't have a name (any node except element and attribute nodes) are evaluated to BOTTOM.
      */
    override def hasName(node: N, name: String): (N, N) = ???

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
    override def getFirst(list: L): N = list.first
  }
}
