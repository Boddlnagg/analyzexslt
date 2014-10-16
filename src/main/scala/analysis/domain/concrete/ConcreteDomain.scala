package analysis.domain.concrete

import analysis.domain.Domain
import analysis.domain.concrete.ConcreteXMLDomain.{L, N}
import analysis.domain.concrete.ConcreteXPathDomain.V
import xml.{XMLAttribute, XMLTextNode, XMLNode}
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

    /** Create an attribute node with the given name and text value.
      * Values that are not strings evaluate to BOTTOM.
      */
    override def createAttribute(name: String, value: V): N = value match {
      case Top => Top
      case Value(StringValue(v)) => Value(XMLAttribute(name, v))
      case _ => Bottom
    }

    /** Create a text node with the given text value.
      * Values that are not strings evaluate to BOTTOM.
      */
    override def createTextNode(value: V): N = value match {
      case Top => Top
      case Value(StringValue(v)) => Value(XMLTextNode(v))
      case _ => Bottom
    }
  }

  object XPATH extends ConcreteXPathDomain.D[N, L] {
    /** Converts a list of nodes to a node-set value.
      * This has to order the nodes in document order and remove duplicates.
      */
    override def toNodeSet(list: L): V = list.map(nodes => NodeSetValue((TreeSet[XMLNode]() ++ nodes).toList))

    /** Match on a value to find out whether it is a node-set value.
      * The part of the value that is a node-set value is returned as a node list in the first result value,
      * the part of the value that isn't is returned in the second result value.
      */
    override def matchNodeSetValues(v: V): (L, V) = v match {
      case Top => (Top, Top)
      case Bottom => (Bottom, Bottom)
      case Value(NodeSetValue(nodes)) => (Value(nodes), Bottom)
      case Value(other) => (Bottom, Value(other))
    }
  }

}
