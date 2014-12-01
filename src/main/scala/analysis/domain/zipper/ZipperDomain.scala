package analysis.domain.zipper

import analysis.domain.Domain
import analysis.domain.Lattice
import analysis.domain.zipper.ZipperXMLDomain.{L, N}
import analysis.domain.zipper.ZipperXMLDomain._
import analysis.domain.powerset.{TypedXPathValue, TypedPowersetXPathDomain}

protected object OuterXPATH extends TypedPowersetXPathDomain[L]

object ZipperDomain extends Domain[N, L, OuterXPATH.V] {
  type V = TypedXPathValue[L]

  override val xmlDom = XML
  override val xpathDom = XPATH

  protected object XML extends ZipperXMLDomain.D[V] {
    override val xpathDom = XPATH

    /** Create a text node with the given text value.
      * Values that are not strings evaluate to BOTTOM.
      */
    override def createTextNode(value: V): N = {
      val desc: Set[NodeDescriptor] = value.str match {
        case None => Set(AnyTextNode)
        case Some(s) => s.map(text => TextNode(text))
      }
      val tree = Subtree(desc, ZNil(), ZNil()) // text nodes have no children or attributes
      val path = Set[Path](DescendantStep(AnyTextNodePath, RootPath))
      (tree, path)
    }

    /** Create an attribute node with the given name and text value.
      * Values that are not strings evaluate to BOTTOM.
      */
    override def createAttribute(name: String, value: V): N = {
      val desc: Set[NodeDescriptor] = value.str match {
        case None => Set(AnyAttributeNode)
        case Some(s) => s.map(text => AttributeNode(name, text))
      }
      val tree = Subtree(desc, ZNil(), ZNil()) // attribute nodes have no children or attributes
      val path = Set[Path](DescendantStep(NamedAttribute(name), RootPath))
      (tree, path)
    }
  }

  protected object XPATH extends OuterXPATH.D[N] {
    override val xmlDom = XML

    /** A node-set is converted to a string by returning the string-value of the node in the node-set that is
      * first in document order. If the node-set is empty, an empty string is returned.
      */
    override def nodeSetToStringValue(nodeSet: L): Option[Set[String]] = nodeSet match {
      // Converting node sets to strings is not implemented for node-sets with more than one element
      case ZBottom() => Some(Set())
      case ZTop() => None
      case ZNil() => Some(Set(""))
      case ZCons(node, ZNil()) => getStringValue(node)
      case ZMaybeNil(node, ZNil()) => getStringValue(node) match {
        case None => None
        case Some(s) => Some(s | Set("")) // add empty string because list may be empty
      }
      case ZUnknownLength(node) => getStringValue(node) match {
        case None => None
        case Some(s) => Some(s | Set("")) // add empty string because list may be empty
      }
    }

    def getStringValue(node: N): Option[Set[String]] = {
      // TODO: this suffers from code duplication with XMLDomain.getStringValue()
      val lat = Lattice.createFromOptionalSet[String]
      def getStringValueFromSubtree(tree: Subtree): Option[Set[String]] = {
        val Subtree(desc, attributes, children) = tree
        lat.joinAll(desc.map {
          case RootNode => getStringValueFromSubtree(children.first)
          case ElementNode(name) => None// TODO: concatenate the string values of all (non-attribute) children
          case AttributeNode(name, value) => Some(Set(value))
          case TextNode(value) => Some(Set(value))
          case CommentNode(value) => Some(Set(value))
          case AnyElementNode | AnyAttributeNode | AnyTextNode | AnyCommentNode => None
        })
      }
      getStringValueFromSubtree(node._1)
    }

    /** Turn a node list into a set by sorting nodes in document order and removing duplicate nodes */
    override def nodeListToSet(list: L): L = list match {
      case ZBottom() => ZBottom()
      case ZTop() => ZTop()
      case ZUnknownLength(node) => list
      case ZCons(head, tail) => ZUnknownLength(list.joinInner) // TODO: this could be made more precise
      case ZMaybeNil(head, tail) => ZUnknownLength(list.joinInner) // TODO: this could be made more precise
      case ZNil() => ZNil()
    }
  }
}
