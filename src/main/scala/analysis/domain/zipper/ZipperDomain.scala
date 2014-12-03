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
        case None => Set(AnyText)
        case Some(s) => s.map(text => Text(text))
      }
      val tree = Subtree(desc, ZNil(), ZNil()) // text nodes have no children or attributes
      val path = Set[Path](DescendantStep(AnyTextNodeStep, RootPath))
      (tree, path)
    }

    /** Create an attribute node with the given name and text value.
      * Values that are not strings evaluate to BOTTOM.
      */
    override def createAttribute(name: String, value: V): N = {
      val desc: Set[NodeDescriptor] = value.str match {
        case None => Set(NamedAttribute(name))
        case Some(s) => s.map(text => Attribute(name, text))
      }
      val tree = Subtree(desc, ZNil(), ZNil()) // attribute nodes have no children or attributes
      val path = Set[Path](DescendantStep(NamedAttributeStep(name), RootPath))
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
      case ZCons(node, ZNil()) => xmlDom.getStringValue(node).str
      case ZMaybeNil(node, ZNil()) => xmlDom.getStringValue(node).str match {
        case None => None
        case Some(s) => Some(s | Set("")) // add empty string because list may be empty
      }
      case ZUnknownLength(node) => xmlDom.getStringValue(node).str match {
        case None => None
        case Some(s) => Some(s | Set("")) // add empty string because list may be empty
      }
    }

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
        // create a list with at least 1 element
        ZCons(inner, ZUnknownLength(inner)) // TODO: can this be made more precise?
      case ZMaybeNil(head, tail) => ZUnknownLength(list.joinInner) // TODO: can this be made more precise?
      case ZNil() => ZNil()
    }
  }
}
