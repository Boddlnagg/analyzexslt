package analysis.domain.zipper

import analysis.domain.{XPathDomain, XMLDomain, Lattice}

/** Just a wrapper for the type aliases */
object ZipperXMLDomain {
  type S = Subtree // type of subtrees
  type P = Set[Path] // type of paths
  type N = (S, P) // a node is a subtree and a path
  type L = ZList[N] // type of node lists

  trait NodeDescriptor
  case object Root extends NodeDescriptor
  case class Element(name: String) extends NodeDescriptor
  case object AnyElement extends NodeDescriptor
  trait AttributeNodeDescriptor extends NodeDescriptor
  case class Attribute(name: String, value: String) extends AttributeNodeDescriptor
  case class NamedAttribute(name: String) extends AttributeNodeDescriptor
  case object AnyAttribute extends AttributeNodeDescriptor
  case class Text(value: String) extends NodeDescriptor {
    override def toString = f"Text(${value.replace("\n", "\\n")})"
  }
  case object AnyText extends NodeDescriptor
  case class Comment(value: String) extends NodeDescriptor
  case object AnyComment extends NodeDescriptor

  case class Subtree(desc: Set[NodeDescriptor], attributes: ZList[Set[NodeDescriptor]], children: ZList[Subtree])

  implicit object SubtreeLattice extends Lattice[Subtree] {
    def top = Subtree(latD.top, ZUnknownLength(Set(AnyAttribute)), ZTop())
    def bottom = Subtree(latD.bottom, ZBottom(), ZBottom())
    def join(left: Subtree, right: Subtree): Subtree =
      Subtree(latD.join(left.desc, right.desc), left.attributes | right.attributes, left.children | right.children)
    def meet(left: Subtree, right: Subtree): Subtree =
      Subtree(latD.meet(left.desc, right.desc), left.attributes & right.attributes, left.children & right.children)
    def lessThanOrEqual(left: Subtree, right: Subtree): Boolean =
      latD.lessThanOrEqual(left.desc, right.desc) &&
        left.attributes <= right.attributes &&
        left.children <= right.children
  }

  private val latS: Lattice[S] = SubtreeLattice // lattice for subtrees
  private val latP = Path.PathSetLattice // lattice for paths
  private val latD = DescriptorLattice // lattice for descriptors

  implicit object DescriptorLattice extends Lattice[Set[NodeDescriptor]] {
    def top = Set(
      Root,
      AnyElement,
      AnyAttribute,
      AnyComment,
      AnyText
    )

    def bottom = Set()

    def join(left: Set[NodeDescriptor], right: Set[NodeDescriptor]): Set[NodeDescriptor] = normalizeDescriptors(left | right)
    def meet(left: Set[NodeDescriptor], right: Set[NodeDescriptor]): Set[NodeDescriptor] = {
      val result = left.cross(right).flatMap {
        case (p1, p2) => meetSingle(p1, p2)
      }
      normalizeDescriptors(result.toSet)
    }

    def lessThanOrEqual(left: Set[NodeDescriptor], right: Set[NodeDescriptor]): Boolean = {
      left.forall(desc1 => right.exists(desc2 => lessThanOrEqualSingle(desc1, desc2)))
    }

    def normalizeDescriptors(set: Set[NodeDescriptor]): Set[NodeDescriptor] = set.toList.foldRight(List[NodeDescriptor]()) {
      case (next, acc) => if (acc.exists(e => lessThanOrEqualSingle(next, e)))
        acc
      else
        next :: acc.filter(e => !lessThanOrEqualSingle(e, next))
    }.toSet

    def lessThanOrEqualSingle(desc1: NodeDescriptor, desc2: NodeDescriptor): Boolean = {
      (desc1, desc2) match {
        case _ if desc1 == desc2 => true // equal
        case (Element(_), AnyElement) => true // less than
        case (Attribute(_, _), AnyAttribute) => true // less than
        case (Attribute(name1, _), NamedAttribute(name2)) if name1 == name2 => true // less than
        case (NamedAttribute(_), AnyAttribute) => true // less than
        case (Text(_), AnyText) => true // less than
        case (Comment(_), AnyComment) => true // less than
        case _ => false
      }
    }

    private def meetSingle(desc1: NodeDescriptor, desc2: NodeDescriptor): Option[NodeDescriptor] = {
      (desc1, desc2) match {
        case _ if desc1 == desc2 => Some(desc1)
        case (n@Element(_), AnyElement) => Some(n)
        case (AnyElement, n@Element(_)) => Some(n)
        case (n@Attribute(_, _), AnyAttribute) => Some(n)
        case (AnyAttribute, n@Attribute(_, _)) => Some(n)
        case (n@Attribute(name1, _), NamedAttribute(name2)) if name1 == name2 => Some(n)
        case (NamedAttribute(name2), n@Attribute(name1, _)) if name1 == name2 => Some(n)
        case (n@NamedAttribute(_), AnyAttribute) => Some(n)
        case (AnyAttribute, n@NamedAttribute(_)) => Some(n)
        case (n@Text(_), AnyText) => Some(n)
        case (AnyText, n@Text(_)) => Some(n)
        case (n@Comment(_), AnyComment) => Some(n)
        case (AnyComment, n@Comment(_)) => Some(n)
        case _ => None // represents BOTTOM here
      }
    }
  }

  implicit object NodeLattice extends Lattice[N] {
    def top = (latS.top, latP.top)
    def bottom = (latS.bottom, latP.bottom)
    def join(left: N, right: N): N = normalize(latS.join(left._1, right._1), latP.join(left._2, right._2)) // TODO: normalize is probably not required here
    def meet(left: N, right: N): N = normalize(latS.meet(left._1, right._1), latP.meet(left._2, right._2))

    def lessThanOrEqual(left: N, right: N): Boolean =
      latS.lessThanOrEqual(left._1, right._1) && latP.lessThanOrEqual(left._2, right._2)
  }

  private def getDescriptorsFromPaths(path: P): Set[NodeDescriptor] = {
    def getSingle(desc: PathStepDescriptor): NodeDescriptor = desc match {
      case AnyElementStep => AnyElement
      case NamedElementStep(name) => Element(name)
      case AnyAttributeStep => AnyAttribute
      case NamedAttributeStep(name) => NamedAttribute(name)
      case AnyTextNodeStep => AnyText
      case AnyCommentNodeStep => AnyComment
    }

    latD.normalizeDescriptors(path.map {
      case RootPath => Root
      case ChildStep(desc, _) => getSingle(desc)
      case DescendantStep(desc, _) => getSingle(desc)
    })
  }

  private def getPathsFromDescriptors(descriptors: Set[NodeDescriptor]): P = descriptors.map {
    case Root => RootPath
    case Element(name) => DescendantStep(NamedElementStep(name), RootPath)
    case AnyElement => DescendantStep(AnyElementStep, RootPath)
    case Attribute(name, _) => DescendantStep(NamedAttributeStep(name), RootPath)
    case NamedAttribute(name) => DescendantStep(NamedAttributeStep(name), RootPath)
    case AnyAttribute => DescendantStep(AnyAttributeStep, RootPath)
    case Text(_) | AnyText => DescendantStep(AnyTextNodeStep, RootPath)
    case Comment(_) | AnyComment => DescendantStep(AnyCommentNodeStep, RootPath)
  }

  /** Removes impossible elements (where Path and Subtree descriptors don't agree) */
  private def normalize(node: N): N = {
    // TODO: further refinements (e.g. if the descriptor only describes nodes that can't have children, set children to ZNil)
    //       or more general: eliminate all children that can not have the descriptor as their parent (recursively?)
    val (Subtree(desc, attributes, children), path) = node
    if (children.isInstanceOf[ZBottom[Subtree]] || attributes.isInstanceOf[ZBottom[Set[NodeDescriptor]]]) {
      NodeLattice.bottom
    } else {
      val meetDesc = latD.meet(getDescriptorsFromPaths(path), desc)
      if (meetDesc == Set()) { // BOTTOM
        NodeLattice.bottom // necessary to make children BOTTOM also (which would not happen in the below case)
      } else {
        val meetPath = latP.meet(getPathsFromDescriptors(desc), path)
        if (meetPath == Set()) {
          NodeLattice.bottom
        } else {
          (Subtree(meetDesc, attributes, children), meetPath)
        }
      }
    }
  }

  val topTree = latS.top

  /** This is the actual (partial) domain implementation */
  trait D[V] extends XMLDomain[N, L, V] {
    val xpathDom: XPathDomain[V, N, L]

    /** Get the TOP element for XML nodes. */
    override def top: N = NodeLattice.top

    /** Gets the BOTTOM element for XML nodes. */
    override def bottom: N = NodeLattice.bottom

    /** Get the TOP element for XML node lists. */
    override def topList: L = ZTop()

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

    /** Create an element node with the given name, attributes and children.
      * The output is created bottom-up, so children are always created before their parent nodes.
      */
    override def createElement(name: String, attributes: L, children: L): N = {
      // TODO: empty text nodes should be filtered out and multiple consecutive ones should be merged

      val attrList = attributes.map(_._1.desc) // get a ZList of attribute descriptors

      // because attributes are regarded as an (unordered) set, we have to represent them as ZUnknownLength, if there are any
      val attrSet: ZList[Set[NodeDescriptor]] = attrList match {
        case ZBottom() => ZBottom()
        case ZNil() => ZNil()
        case _ =>
          val allPossibleAttributes = attrList.joinInner
          if (allPossibleAttributes == Set())
            ZBottom()
          else
            ZUnknownLength(allPossibleAttributes)
      }

      val tree = Subtree(Set(Element(name)), attrSet, children.map(_._1))
      val path = Set[Path](DescendantStep(AnyElementStep, RootPath))
      (tree, path)
    }

    /** Create an emtpy list containing no nodes */
    override def createEmptyList(): L = ZNil()

    /** Create a list containing a single abstract node */
    override def createSingletonList(node: N): L = ZCons(node, ZNil())

    /** Get the root node of a given node */
    override def getRoot(node: N): N = {
      val (Subtree(desc, attributes, children), path) = node
      val root: P = Set(RootPath)
      // NOTE: we use the empty attribute set because root node can't have attributes
      // TODO: the child could be more precise using information from the path
      normalize(Subtree(latD.meet(desc, getDescriptorsFromPaths(root)), ZNil(), ZCons(latS.top, ZNil())), latP.meet(path, root))
    }
    // TODO: this might be implementable using getParent() and isRoot()

    /** Get the list of attributes of a given node.
      * Nodes that are not an element (and therefore don't have attributes) return an empty list, not BOTTOM! */
    override def getAttributes(node: N): L = {
      val (Subtree(desc, attributes, children), path) = node
      val attributePath: Set[Path] = latP.getAttributes(path).joinInner
      attributes.map(a => normalize(Subtree(a, ZNil(), ZNil()),attributePath))
    }

    /** Get the list of children of a given node.
      * Root nodes have a single child, element nodes have an arbitrary number of children.
      * Nodes that don't have children return an empty list, not BOTTOM! */
    override def getChildren(node: N): L = {
      val (Subtree(desc, attributes, children), path) = node
      val childrenPath: Set[Path] = latP.getChildren(path).joinInner
      children.map(tree => normalize(tree, childrenPath))
    }

    /** Get the parent of given node. If the node has no parent (root node), BOTTOM is returned. */
    override def getParent(node: N): N = {
      val (Subtree(desc, attributes, children), path) = node
      val parent = latP.getParent(path)
      if (parent == Set(RootPath)) { // parent must be root node
        // root has exactly one child, which must be the node itself
        val newChildren: ZList[Subtree] = ZCons(node._1, ZNil())
        val newAttributes: ZList[Set[NodeDescriptor]] = ZNil() // root has no attributes
        normalize(Subtree(getDescriptorsFromPaths(parent), newAttributes, newChildren), parent)
      } else { // parent could be any element or root
        val newChildren: ZList[Subtree] = ZUnknownLength(isAttribute(top)._2._1) // don't know anything about siblings of `node`, except that they are not attributes
        val newAttributes: ZList[Set[NodeDescriptor]] = ZUnknownLength(Set(AnyAttribute)) // don't know anything about attributes of parent, except that they are attributes
        normalize(Subtree(getDescriptorsFromPaths(parent), newAttributes, newChildren), parent)
      }
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
    override def partitionAttributes(list: L): (L, L) =
      (list.takeWhile(n => isAttribute(n)._1), list.filter(n => isAttribute(n)._2))

    /** Wraps a list of nodes in a document/root node. Lists that don't have exactly one element evaluate to BOTTOM. */
    override def wrapInRoot(list: L): N = {
      val firstChild: N = list match {
        case ZTop() => top
        case ZBottom() => bottom
        case ZUnknownLength(elems) => elems
        case ZCons(first, ZCons(_, _)) => bottom // list with more than one element
        case ZCons(first, _) => first // list with at least one element
        case ZMaybeNil(first, ZCons(_, _)) => bottom // list with 0 or more than one element (at least 2)
        case ZMaybeNil(first, _) => first // list with 0 or more elements (can't know exactly)
        case ZNil() => bottom // list with 0 elements
      }
      val (firstChildElement, _) = isElement(firstChild)
      normalize(Subtree(Set(Root), ZNil(), ZList(List(firstChildElement._1))), Set(RootPath))
    }

    /** Copies a list of nodes, so that they can be used in the output.
      * A root node is copied by copying its child (not wrapped in a root node). */
    override def copyToOutput(list: L): L = list.map {
      case in@(Subtree(desc, attributes, children), path) =>
        val (root, notRoot) = isRoot(in)
        if (!lessThanOrEqual(root, bottom)) { // isRoot is not BOTTOM -> node might be a root node
          val child = getChildren(in).first
          val (tree, _) = join(notRoot, child)
          normalize(tree, latP.top)
        } else {
          normalize(Subtree(desc, attributes, children), latP.top)
        }
    }

    /** Evaluates a function for every element in the given list, providing also the index of each element in the list.
      * The resulting lists are flattened into a single list.
      */
    override def flatMapWithIndex(list: L, f: (N, V) => L): L = {
      def flatMapWithIndexInternal(rest: L, currentIndex: V, f: (N, V) => L): L = rest match {
        case ZBottom() => ZBottom()
        case ZTop() => ZTop()
        case ZUnknownLength(elems) => ZUnknownLength(f(elems, xpathDom.topNumber).joinInner)
        case ZCons(first, rest) =>
          f(first, currentIndex) ++ flatMapWithIndexInternal(rest, xpathDom.add(currentIndex, xpathDom.liftNumber(1)), f)
        case ZMaybeNil(first, rest) =>
          ZNil() | f(first, currentIndex) ++ flatMapWithIndexInternal(rest, xpathDom.add(currentIndex, xpathDom.liftNumber(1)), f)
        case ZNil() => ZNil()
      }

      flatMapWithIndexInternal(list, xpathDom.liftNumber(0), f)
    }

    /** Gets the size of a node list */
    override def getNodeListSize(list: L): V = list match {
      case ZBottom() => xpathDom.bottom
      case ZTop() => xpathDom.topNumber
      case ZUnknownLength(elems) => xpathDom.topNumber
      case ZCons(first, rest) => xpathDom.add(xpathDom.liftNumber(1), getNodeListSize(rest))
      case ZMaybeNil(first, rest) => xpathDom.join(xpathDom.liftNumber(0), xpathDom.add(xpathDom.liftNumber(1), getNodeListSize(rest)))
      case ZNil() => xpathDom.liftNumber(0)
    }

    /** Gets the string-value of a node, as specified in the XSLT specification */
    override def getStringValue(node: N): V = {
      def getStringValueFromSubtree(tree: Subtree): V = {
        val Subtree(desc, attributes, children) = tree
        xpathDom.join(desc.map {
          case Root => getStringValueFromSubtree(children.first)
          case Element(name) => xpathDom.topString // TODO: concatenate the string values of all (non-attribute) children
          case Attribute(name, value) => xpathDom.liftLiteral(value)
          case Text(value) => xpathDom.liftLiteral(value)
          case Comment(value) => xpathDom.liftLiteral(value)
          case AnyElement | AnyAttribute | NamedAttribute(_) | AnyText | AnyComment => xpathDom.topString
        })
      }
      getStringValueFromSubtree(node._1)
    }

    /** Predicate function that checks whether a node is a root node.
      * The first result is a node that is known to be a root node (this is BOTTOM if the node definitely
      * is not a root node), the second result is a node that might not be a root node (this is
      * BOTTOM if the node definitely is a root node). The two results are not necessarily disjoint.
      */
    override def isRoot(node: N): (N, N) = {
      val (Subtree(desc, attributes, children), path) = node
      // NOTE: root node can't have attributes, so we set it to ZNil
      val positiveResult: N = normalize(Subtree(latD.meet(desc, Set(Root)), ZNil(), children), latP.meet(path, Set(RootPath)))
      val negativeDesc = desc.diff(Set(Root))
      val negativeResult: N = normalize(Subtree(negativeDesc, attributes, children), path.diff(Set(RootPath)))
      (positiveResult, negativeResult)
    }

    /** Predicate function that checks whether a node is an element node.
      * The first result is a node that is known to be an element node (this is BOTTOM if the node definitely
      * is not an element node), the second result is a node that might not be an element node (this is
      * BOTTOM if the node definitely is an element node). The two results are not necessarily disjoint.
      */
    override def isElement(node: N): (N, N) = {
      val (Subtree(desc, attributes, children), path) = node
      val (pathYes, pathNo) = latP.isElement(path)
      val (descYes, descNo) = desc.partition {
        case Element(_) => true
        case AnyElement => true
        case _ => false
      }
      // TODO: maybe move normalizeDescriptors call into normalize()
      val yes = normalize(Subtree(latD.normalizeDescriptors(descYes), attributes, children), pathYes)
      // NOTE: only elements can have attributes, therefore we use the empty attribute set in the negative result
      val no = normalize(Subtree(latD.normalizeDescriptors(descNo), ZNil(), children), pathNo)
      (yes, no)
    }

    /** Predicate function that checks whether a node is a text node.
      * The first result is a node that is known to be a text node (this is BOTTOM if the node definitely
      * is not a text node), the second result is a node that might not be a text node (this is
      * BOTTOM if the node definitely is a text node). The two results are not necessarily disjoint.
      */
    override def isTextNode(node: N): (N, N) = {
      val (Subtree(desc, attributes, children), path) = node
      val (pathYes, pathNo) = latP.isTextNode(path)
      val (descYes, descNo) = desc.partition {
          case Text(_) => true
          case AnyText => true
          case _ => false
        }
      // NOTE: text nodes cannot have attributes or children, therefore we use the empty attribute set in the positive result
      val yes = normalize(Subtree(latD.normalizeDescriptors(descYes), ZNil(), ZNil()), pathYes)
      val no = normalize(Subtree(latD.normalizeDescriptors(descNo), attributes, children), pathNo)
      (yes, no)
    }

    /** Predicate function that checks whether a node is a comment node.
      * The first result is a node that is known to be a comment node (this is BOTTOM if the node definitely
      * is not a comment node), the second result is a node that might not be a comment node (this is
      * BOTTOM if the node definitely is a comment node). The two results are not necessarily disjoint.
      */
    override def isComment(node: N): (N, N) = {
      val (Subtree(desc, attributes, children), path) = node
      val (pathYes, pathNo) = latP.isComment(path)
      val (descYes, descNo) = desc.partition {
          case Comment(_) => true
          case AnyComment => true
          case _ => false
        }
      // NOTE: comment nodes cannot have attributes or children, therefore we use the empty attribute set in the positive result
      val yes = normalize(Subtree(latD.normalizeDescriptors(descYes), ZNil(), ZNil()), pathYes)
      val no = normalize(Subtree(latD.normalizeDescriptors(descNo), attributes, children), pathNo)
      (yes, no)
    }

    /** Predicate function that checks whether a node is an attribute node.
      * The first result is a node that is known to be an attribute node (this is BOTTOM if the node definitely
      * is not an attribute node), the second result is a node that might not be an attribute node (this is
      * BOTTOM if the node definitely is an attribute node). The two results are not necessarily disjoint.
      */
    override def isAttribute(node: N): (N, N) = {
      val (Subtree(desc, attributes, children), path) = node
      val (pathYes, pathNo) = latP.isAttribute(path)
      val (descYes, descNo) = desc.partition {
          case Attribute(_, _) => true
          case NamedAttribute(_) => true
          case AnyAttribute => true
          case _ => false
        }
      // NOTE: attribute nodes cannot have attributes or children, therefore we use the empty attribute set in the positive result
      val yes = normalize(Subtree(latD.normalizeDescriptors(descYes), ZNil(), ZNil()), pathYes)
      val no = normalize(Subtree(latD.normalizeDescriptors(descNo), attributes, children), pathNo)
      (yes, no)
    }

    /** Predicate function that checks whether a node has a specified name.
      * The first result is a node that is known to have that name (this is BOTTOM if the node definitely
      * doesn't have that name), the second result is a node that might not have that name (this is
      * BOTTOM if the node definitely does have that name). The two results are not necessarily disjoint.
      * Nodes that don't have a name (any node except element and attribute nodes) are evaluated to BOTTOM.
      */
    override def hasName(node: N, name: String): (N, N) = {
      val (Subtree(desc, attributes, children), path) = node
      val (pathYes, pathNo) = latP.hasName(path, name)
      val descYes: Set[NodeDescriptor] = desc.collect {
        case e@Element(n) if name == n => e
        case AnyElement => Element(name)
        case a@Attribute(n, _) if name == n => a
        case a@NamedAttribute(n) if name == n => a
        case AnyAttribute => NamedAttribute(name)
      }

      val descNo: Set[NodeDescriptor] = desc.collect {
        case e@Element(n) if name != n => e
        case AnyElement => AnyElement // can't represent element that does *not* have specified name
        case a@Attribute(n, _) if name != n => a
        case a@NamedAttribute(n) if name != n => a
        case AnyAttribute => AnyAttribute // can't represent attribute that does *not* have specified name
      }

      val yes = normalize(Subtree(latD.normalizeDescriptors(descYes), attributes, children), pathYes)
      val no = normalize(Subtree(latD.normalizeDescriptors(descNo), attributes, children), pathNo)

      (yes, no)
    }

    /** Get the name for a given node. Nodes that don't have a name (i.e. are not an element or attribute node)
      * are evaluated to the empty string, not BOTTOM!
      */
    override def getNodeName(node: N): V = {
      val (Subtree(desc, attributes, children), path) = node
      xpathDom.join(desc.map {
        case Element(name) => xpathDom.liftLiteral(name)
        case AnyElement => xpathDom.topString
        case Attribute(name, _) => xpathDom.liftLiteral(name)
        case NamedAttribute(name) => xpathDom.liftLiteral(name)
        case AnyAttribute => xpathDom.topString
        case _ => xpathDom.liftLiteral("")
      })
    }

    /** Concatenates the values of all text nodes in the list. List elements that are not text nodes are ignored. */
    override def getConcatenatedTextNodeValues(list: L): V = xpathDom.topString // TODO: this is used for attribute values

    /** Filters a list using a given predicate function. The predicate function should never return a node
      * (as its first result) that is less precise than the input node.
      */
    override def filter(list: L, predicate: N => (N, N)): L = list.filter(n => predicate(n)._1)

    /** Gets the first node out of a node list. BOTTOM is returned if the list is empty or BOTTOM. */
    override def getFirst(list: L): N = list.first
  }
}
