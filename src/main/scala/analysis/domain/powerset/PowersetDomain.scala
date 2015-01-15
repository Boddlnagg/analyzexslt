package analysis.domain.powerset

import analysis.domain.Domain
import analysis.domain.powerset.PowersetXMLDomain.{L, N}
import analysis.domain.powerset.PowersetXPathDomain.V
import xml._
import xpath._

import scala.collection.immutable.TreeSet

/** This glues together the PowersetXMLDomain and the PowersetXPathDomain and provides
  * the remaining method implementations.
  */
object PowersetDomain extends Domain[N, L, V] {

  override val xmlDom = XML
  override val xpathDom = XPATH

  protected object XML extends PowersetXMLDomain.D[V] {
    override val xpathDom = XPATH

    override def createElement(name: V, attributes: L, children: L): N = (name, attributes, children) match {
      // TODO: order is wrong (TOP x BOTTOM = BOTTOM); name is missing from cases
      case (Left(_), _) => None
      case (_, Left(_)) => None
      case (Right(s1), Right(s2)) => Some(s1.cross(s2).map {
        case (attr, chld) => XMLElement(name,
          attr.map(a => a.asInstanceOf[XMLAttribute].copy),
          chld.map(c => c.copy))
      }.toSet)
    }

    override def createAttribute(name: V, value: V): N = (name, value) match {
      case (None, None) => None
      case (None, Some(s)) => if (s.collect { case StringValue(_) => () }.isEmpty) BOT else None
      case (Some(s), None) => if (s.collect { case StringValue(_) => () }.isEmpty) BOT else None
      case (Some(s1), Some(s2)) => Some(s1.cross(s2).collect {
        case (StringValue(n), StringValue(v)) => XMLAttribute(n, v)
        // NOTE: other XPath values are evaluated to bottom implicitly
      }.toSet)
    }

    override def createTextNode(value: V): N = value match {
      case None => None
      case Some(s) => Some(s.collect {
        case StringValue(str) if str != "" => XMLTextNode(str)
        // NOTE: other value types are implicitly evaluated to bottom
      })
    }

    override def createComment(value: V): N = value match {
      case None => None
      case Some(s) => Some(s.collect {
        case StringValue(str) => XMLComment(str)
        // NOTE: other value types are implicitly evaluated to bottom
      })
    }
  }

  protected object XPATH extends PowersetXPathDomain.D[N, L] {

    override def toNodeSet(list: L): V = list match {
      case Left(_) => None
      case Right(s) => Some(s.map(nodes => NodeSetValue(nodes.to[TreeSet])))
    }

    override def matchNodeSetValues(v: V): (L, V) = v match {
      case None => (Left(None), None)
      case Some(s) =>
        val nodeSetContents = s.collect {
          case NodeSetValue(nodes) => nodes.toList
        }
        val rest = s.collect {
          case v@(NumberValue(_) | StringValue(_) | BooleanValue(_)) => v
        }
        (Right(nodeSetContents), Some(rest))
    }
  }
}
