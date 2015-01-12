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
      val path = Set[Path](DescendantStep(AnyTextNodeStep, RootPath))
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
      val path = Set[Path](DescendantStep(AnyCommentNodeStep, RootPath))
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
