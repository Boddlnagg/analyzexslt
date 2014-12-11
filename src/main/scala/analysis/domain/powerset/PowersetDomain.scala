package analysis.domain.powerset

import analysis.domain.Domain
import analysis.domain.powerset.PowersetXMLDomain.{L, N}
import analysis.domain.powerset.PowersetXPathDomain.V
import xml.{XMLComment, XMLAttribute, XMLTextNode, XMLNode}
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

    override def createAttribute(name: String, value: V): N = value match {
      case None => None
      case Some(s) => Some(s.collect {
        case StringValue(str) => XMLAttribute(name, str)
        // NOTE: other XPath values are evaluated to bottom implicitly
      })
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
      case Right(s) => Some(s.map(nodes => NodeSetValue((TreeSet[XMLNode]() ++ nodes).toList)))
    }

    override def matchNodeSetValues(v: V): (L, V) = v match {
      case None => (Left(None), None)
      case Some(s) =>
        val nodeSetContents = s.collect {
          case NodeSetValue(nodes) => nodes
        }
        val rest = s.collect {
          case v@(NumberValue(_) | StringValue(_) | BooleanValue(_)) => v
        }
        (Right(nodeSetContents), Some(rest))
    }
  }
}
