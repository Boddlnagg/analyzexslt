package analysis.domain.powerset

import analysis.domain.Domain
import analysis.domain.powerset.PowersetXMLDomain.{L, N}
import analysis.domain.powerset.PowersetXPathDomain.V
import xml.{XMLAttribute, XMLTextNode, XMLNode}
import xpath._

import scala.collection.immutable.TreeSet

/** This glues together the PowersetXMLDomain and the PowersetXPathDomain and provides
  * the remaining method implementations.
  */
object PowersetDomain extends Domain[N, L, V] {

  override val xmlDom = XML
  override val xpathDom = XPATH

  object XML extends PowersetXMLDomain.D[V] {
    override val xpathDom = XPATH

    override def flatMapWithIndex(list: L, f: (N, V) => L): L = list match {
      case None => None
      case Some(s) => xmlDom.listJoin(s.map { l =>
        val mapped = l.zipWithIndex.map { case (n, i) => f(Some(Set(n)), Some(Set(NumberValue(i)))) }
        val flattened = mapped.foldLeft(xmlDom.liftList(Nil))((acc, next) => xmlDom.listConcat(acc, next))
        flattened
      }.toList)
    }

    override def liftAttribute(name: String, value: V): N = value match {
      case None => None
      case Some(s) => Some(s.collect {
        case StringValue(str) => XMLAttribute(name, str)
        // NOTE: other XPath values are evaluated to bottom implicitly
      })
    }

    override def liftTextNode(value: V): N = value match {
      case None => None
      case Some(s) => Some(s.collect {
        case StringValue(str) => XMLTextNode(str)
        // NOTE: other value types are implicitly evaluated to bottom
      })
    }
  }

  object XPATH extends PowersetXPathDomain.D[N, L] {

    override def toNodeSet(set: L): V = {
      set.map(_.map(nodes => NodeSetValue((TreeSet[XMLNode]() ++ nodes).toList)))
    }

    override def matchNodeSetValues(value: V): (L, V) = value match {
      case None => (None, None)
      case Some(s) =>
        val nodeSetContents = Some(s.collect {
          case NodeSetValue(nodes) => nodes
        })
        val rest = Some(s.collect {
          case v@(NumberValue(_) | StringValue(_) | BooleanValue(_)) => v
        })
        (nodeSetContents, rest)
    }
  }
}
