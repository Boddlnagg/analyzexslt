package analysis.domain.concrete

import analysis.domain.Domain
import analysis.domain.concrete.ConcreteXMLDomain.{L, N}
import analysis.domain.concrete.ConcreteXPathDomain.V
import xml._
import xpath._

import scala.collection.immutable.TreeSet

/** This glues together the ConcreteXMLDomain and the ConcreteXPathDomain and provides
  * the remaining method implementations.
  */
object ConcreteDomain extends Domain[N, L, V] {
  override val xmlDom = XML
  override val xpathDom = XPATH

  object XML extends ConcreteXMLDomain.D[V] {
    override val xpathDom = XPATH

    /** Create an element node with the given name, attributes and children.
      * The output is created bottom-up, so children are always created before their parent nodes.
      * Consecutive text node children must be merged into a single text node by this method.
      */
    override def createElement(name: V, attributes: L, children: L): N = (name, attributes, children) match {
      case (Value(StringValue(n)), Value(attr), Value(chld)) =>
        try Value(XMLElement(n, attr.map(_.asInstanceOf[XMLAttribute]), chld))
        catch {
          case e: ClassCastException => Bottom
          case e: IllegalArgumentException => Bottom
        }
      case (Value(v), _, _) if !v.isInstanceOf[StringValue] => Bottom // wrong type of first argument
      case (_, Bottom, _) => Bottom
      case (_, _, Bottom) => Bottom
      case _ => Top
    }

    /** Create an attribute node with the given name and text value.
      * Values that are not strings evaluate to BOTTOM.
      */
    override def createAttribute(name: V, value: V): N = (name, value) match {
      case (Value(StringValue(n)), Value(StringValue(v))) => Value(XMLAttribute(n, v))
      case (Top, Top) => Top
      case (Top, Value(StringValue(_))) => Top
      case (Value(StringValue(_)), Top) => Top
      case _ => Bottom
    }

    /** Create a text node with the given text value. Values that are not strings evaluate to BOTTOM.
      * The empty string also evaluates to BOTTOM, because text nodes with no content are not allowed.
      */
    override def createTextNode(value: V): N = value match {
      case Top => Top
      case Value(StringValue(str)) if str != ""  => Value(XMLTextNode(str))
      case _ => Bottom
    }

    /** Create a comment node with the given text value. Values that are not strings evaluate to BOTTOM. */
    override def createComment(value: V): N = value match {
      case Top => Top
      case Value(StringValue(str)) => Value(XMLComment(str))
      case _ => Bottom
    }
  }

  object XPATH extends ConcreteXPathDomain.D[N, L] {
    /** Converts a list of nodes to a node-set value.
      * This has to order the nodes in document order and remove duplicates.
      */
    override def toNodeSet(list: L): V = list.map(nodes => NodeSetValue(nodes.to[TreeSet]))

    /** Match on a value to find out whether it is a node-set value.
      * The part of the value that is a node-set value is returned as a node list in the first result value,
      * the part of the value that isn't is returned in the second result value.
      */
    override def matchNodeSetValues(v: V): (L, V) = v match {
      case Top => (Top, Top)
      case Bottom => (Bottom, Bottom)
      case Value(NodeSetValue(nodes)) => (Value(nodes.toList), Bottom)
      case Value(other) => (Bottom, Value(other))
    }
  }

}
