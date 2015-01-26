package analysis.domain.zipper

import analysis.domain.Domain
import analysis.domain.Lattice
import analysis.domain.zipper.ZipperXMLDomain.{L, N}
import analysis.domain.zipper.ZipperXMLDomain._
import analysis.domain.powerset.{TypedXPathValue, TypedPowersetXPathDomain}

protected object OuterXPATH extends TypedPowersetXPathDomain[L]

/** This glues together the ZipperXMLDomain and the TypedPowersetXPathDomain and provides
  * the remaining method implementations.
  */
object ZipperDomain extends Domain[N, L, OuterXPATH.V] {
  type V = TypedXPathValue[L]

  override val xmlDom = XML
  override val xpathDom = XPATH

  protected object XML extends ZipperXMLDomain.D[V] {
    override val xpathDom = XPATH

    /** Create an element node with the given name, attributes and children.
      * The output is created bottom-up, so children are always created before their parent nodes.
      * Consecutive text node children must be merged into a single text node by this method.
      */
    override def createElement(name: V, attributes: L, children: L): N = {

      /** This helper function merges consecutive text nodes in the given list into single text nodes.
        * The first result is the list with consecutive text nodes merged, the second is a boolean that
        * is true iff the first element of input list might be a text node.
        * By returning ZUnknownLength in many cases, it is less precise than it could be, but doing it
        * right would require a LOT of work.
        */
      def mergeConsecutiveTextNodes(list: ZListElement[N]): (ZListElement[N], Boolean) = {
        // a node that represents any text node
        val anyTextNode: N = normalize((Subtree(Set(AnyText), ZNil(), ZNil()),
          Set(DescendantStep(AnyTextNodeStep, RootPath(isFragment = false)), DescendantStep(AnyTextNodeStep, RootPath(isFragment = true)))))

        list match {
          case ZNil() => (ZNil(), false)
          case ZCons(first, rest) =>
            val (restResult, restContainsText) = mergeConsecutiveTextNodes(rest)
            val (text, _) = isTextNode(first)
            val firstContainsText = !lessThanOrEqual(text, bottom)
            if (firstContainsText && restContainsText) {
              // there are two consecutive list elements that might be text nodes and must be merged
              val joined = joinAll(List(anyTextNode, first, restResult.joinInner))
              if (lessThanOrEqual(joined, anyTextNode)) // joined == anyTextNode, because joined is already <= anyTextNode
                (ZCons(anyTextNode, ZNil()), true)
              else
                (ZUnknownLength(joined), true)
            } else {
              (ZCons(first, restResult), firstContainsText)
            }
          case ZMaybeNil(first, rest) =>
            val (restResult, restContainsText) = mergeConsecutiveTextNodes(rest)
            val (text, _) = isTextNode(first)
            val firstContainsText = !lessThanOrEqual(text, bottom)
            if (firstContainsText && restContainsText) {
              // there are two consecutive list elements that might be text nodes and must be merged
              val joined = joinAll(List(anyTextNode, first, restResult.joinInner))
              if (lessThanOrEqual(joined, anyTextNode)) // joined == anyTextNode, because joined is already <= anyTextNode
                (ZMaybeNil(anyTextNode, ZNil()), true)
              else
                (ZUnknownLength(joined), true)
            } else {
              (ZMaybeNil(first, restResult), firstContainsText)
            }
          case ZUnknownLength(elems) =>
            val (text, _) = isTextNode(elems)
            val elemsContainText = !lessThanOrEqual(text, bottom)
            if (elemsContainText)
              (ZUnknownLength(join(anyTextNode, elems)), elemsContainText)
            else
              (ZUnknownLength(elems), elemsContainText)
          case ZTop() => (ZTop(), true)
        }
      }

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

      val newChildren: L = children match {
        case ZBottom() => ZBottom()
        case res => mergeConsecutiveTextNodes(res.asInstanceOf[ZListElement[N]])._1
      }

      val desc: Set[NodeDescriptor] = name.str match {
        case None => Set(AnyElement)
        case Some(s) => s.map { n => Element(n) }
      }

      val tree = Subtree(desc, attrSet, newChildren.map(_._1))
      val path = Set[Path](DescendantStep(AnyElementStep, RootPath(isFragment = false)), DescendantStep(AnyElementStep, RootPath(isFragment = true)))
      normalize(tree, path)
    }

    /** Create a text node with the given text value. Values that are not strings evaluate to BOTTOM.
      * The empty string also evaluates to BOTTOM, because text nodes with no content are not allowed.
      */
    override def createTextNode(value: V): N = {
      val desc: Set[NodeDescriptor] = value.str match {
        case None => Set(AnyText)
        case Some(s) => s.collect {
          case str if str != "" => Text(str)
        }
      }
      val tree = Subtree(desc, ZNil(), ZNil()) // text nodes have no children or attributes
      val path = Set[Path](DescendantStep(AnyTextNodeStep, RootPath(isFragment = false)), DescendantStep(AnyTextNodeStep, RootPath(isFragment = true)))
      (tree, path)
    }

    /** Create a comment node with the given text value. Values that are not strings evaluate to BOTTOM. */
    override def createComment(value: V): N = {
      val desc: Set[NodeDescriptor] = value.str match {
        case None => Set(AnyComment)
        case Some(s) => s.collect {
          case str => Comment(str)
        }
      }
      val tree = Subtree(desc, ZNil(), ZNil()) // comment nodes have no children or attributes
      val path = Set[Path](DescendantStep(AnyCommentNodeStep, RootPath(isFragment = false)), DescendantStep(AnyCommentNodeStep, RootPath(isFragment = true)))
      (tree, path)
    }

    /** Create an attribute node with the given name and text value.
      * Values that are not strings evaluate to BOTTOM.
      */
    override def createAttribute(name: V, value: V): N = {
      val desc: Set[NodeDescriptor] = (name.str, value.str) match {
        case (Some(s), _) if s.isEmpty => Set()
        case (_, Some(s)) if s.isEmpty => Set()
        case (None, _) => Set(AnyAttribute) // don't know the name
        case (Some(s1), None) => s1.map(n => NamedAttribute(n)) // know the name but not the value
        case (Some(s1), Some(s2)) => s1.cross(s2).map { case (n, v) => Attribute(n, v)}.toSet
      }
      val tree = Subtree(desc, ZNil(), ZNil()) // attribute nodes have no children or attributes
      val path: Set[Path] = name.str match {
          case None => Set[Path](DescendantStep(AnyAttributeStep, RootPath(isFragment = false)), DescendantStep(AnyAttributeStep, RootPath(isFragment = true)))
          case Some(s) => s.flatMap(n => Set(DescendantStep(NamedAttributeStep(n), RootPath(isFragment = false)), DescendantStep(NamedAttributeStep(n), RootPath(isFragment = true))))
      }
      (tree, path)
    }
  }

  protected object XPATH extends OuterXPATH.D[N] {
    override val xmlDom = XML

    /** Turn a node list into a set by sorting nodes in document order and removing duplicate nodes */
    override def nodeListToSet(list: L): L = list match {
      case ZBottom() => ZBottom()
      case ZTop() => ZTop()
      // NOTE: the following two are special cases, as singleton and empty lists don't change when being converted to a sorted set
      case ZCons(_, ZNil()) => list
      case ZMaybeNil(_, ZNil()) => list
      case ZUnknownLength(node) => list
      case ZCons(head, tail) =>
        val inner = list.joinInner
        // create a list with at least 1 element (we don't know their order, because they must be in document order
        // and we can't reconstruct the original order from the information we have in this domain)
        ZCons(inner, ZUnknownLength(inner))
      case ZMaybeNil(head, tail) => ZUnknownLength(list.joinInner)
      case ZNil() => ZNil()
    }
  }
}
