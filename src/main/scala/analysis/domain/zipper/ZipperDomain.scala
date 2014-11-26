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
      def getStringValueFromSubtree(tree: ZipperTree): Option[Set[String]] = {
        val ZipperTree(desc, children) = tree
        desc match {
          case None => None
          case Some(s) => lat.joinAll(s.map {
            case RootNode => getStringValueFromSubtree(children.first)
            case ElementNode(name) => None// TODO: concatenate the string values of all (non-attribute) children
            case AttributeNode(name, value) => Some(Set(value))
            case TextNode(value) => Some(Set(value))
            case CommentNode(value) => Some(Set(value))
          })
        }
      }
      getStringValueFromSubtree(node._1)
    }

    /** Turn a node list into a set by sorting nodes in document order and removing duplicate nodes */
    override def nodeListToSet(list: L): L = ???
  }
}
