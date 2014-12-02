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
    override def createTextNode(value: V): N = createTextNodeInternal(value.str)

    private def createTextNodeInternal(text: Option[Set[String]]) = {
      val desc: Set[NodeDescriptor] = text match {
        case None => Set(AnyText)
        case Some(s) => s.map(t => Text(t))
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
        case None => Set(AnyAttribute)
        case Some(s) => s.map(text => Attribute(name, text))
      }
      val tree = Subtree(desc, ZNil(), ZNil()) // attribute nodes have no children or attributes
      val path = Set[Path](DescendantStep(NamedAttributeStep(name), RootPath))
      (tree, path)
    }

    /** Appends text to a node list.
      * Empty strings are ignored (return the unmodified list) and text that immediately follows
      * an existing text node is merged into that text node.
      */
    override def appendText(list: L, text: V): L = {
      // TODO: write unit tests for appendText
      def appendTextToNode(textNode: N, text: Option[Set[String]]): N = {
        val (Subtree(desc, attributes, children), paths) = textNode
        val newDesc : Set[NodeDescriptor] = if (text == None || desc.contains(AnyText)) {
          Set(AnyText)
        } else {
          desc.map {
            case Text(t) => text.get.map(toAppend =>  Text(t + toAppend))
          }.flatten
        }
        // NOTE: because paths never store the content of text nodes, we can just use the original paths
        normalize(Subtree(newDesc, attributes, children), paths)
      }

      def appendTextInternal(list: L, text: Option[Set[String]]): L = list match {
        case ZTop() => ZTop()
        case ZUnknownLength(elem) =>
          val (elemText, elemNotText) = isTextNode(elem)
          if (lessThanOrEqual(elem, elemText)) // elem is definitely a text node and nothing else (but list might be empty)
            ZUnknownLength(join(createTextNodeInternal(text), appendTextToNode(elemText, text)))
          else if (lessThanOrEqual(elemText, bottom)) // elem is definitely not a text node
            ZUnknownLength(join(createTextNodeInternal(text), elem))
          else // elem might or might not be a text node
            ZUnknownLength(join(List(elemNotText, createTextNodeInternal(text), appendTextToNode(elemText, text))))
        case ZNil() => ZCons(createTextNodeInternal(text), ZNil())
        case ZMaybeNil(f, r) => // this case can only apply for the original list, because recursive calls aways reduce ZMaybeNil to ZCons
          ZCons(createTextNodeInternal(text), ZNil()) | appendTextInternal(ZCons(f, r), text)
        case ZCons(last, ZNil()) =>
          val (lastText, lastNotText) = isTextNode(last)
          if (lessThanOrEqual(last, lastText)) // last is definitely a text node and nothing else
            ZCons(appendTextToNode(lastText, text), ZNil())
          else if (lessThanOrEqual(lastText, bottom)) // last is definitely not a text node
            ZCons(last, ZCons(createTextNodeInternal(text), ZNil()))
          else // last might or might not be a text node
            ZCons(join(lastNotText, appendTextToNode(lastText, text)), ZMaybeNil(createTextNodeInternal(text), ZNil()))
        case ZCons(maybeLast, ZMaybeNil(f, r)) =>
          val (lastText, lastNotText) = isTextNode(maybeLast)
          val resultIfNotLast = ZCons(maybeLast, appendTextInternal(ZCons(f, r), text).asInstanceOf[ZListElement[N]])
          if (lessThanOrEqual(maybeLast, lastText)) // maybeLast is definitely a text node and nothing else
            ZCons(appendTextToNode(lastText, text), ZNil()) | resultIfNotLast
          else if (lessThanOrEqual(lastText, bottom)) // maybeLast is definitely not a text node
            ZCons(maybeLast, ZCons(createTextNodeInternal(text), ZNil())) | resultIfNotLast
          else // maybeLast might or might not be a text node
            ZCons(join(lastNotText, appendTextToNode(lastText, text)), ZMaybeNil(createTextNodeInternal(text), ZNil())) | resultIfNotLast
        case ZCons(first, rest) => ZCons(first, appendTextInternal(rest, text).asInstanceOf[ZListElement[N]])
      }

      if (text.str == Some(Set()) || list.isInstanceOf[ZBottom[N]]) // text or list is BOTTOM
        ZBottom()
      else if (text.str == Some(Set(""))) // text is only the empty string -> return original list
        list
      else
        appendTextInternal(list, text.str.map(_.filter(s => s != ""))) // extract string part of value and filter out empty strings
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
          case Root => getStringValueFromSubtree(children.first)
          case Element(name) => None// TODO: concatenate the string values of all (non-attribute) children
          case Attribute(name, value) => Some(Set(value))
          case Text(value) => Some(Set(value))
          case Comment(value) => Some(Set(value))
          case AnyElement | AnyAttribute | AnyText | AnyComment => None
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
