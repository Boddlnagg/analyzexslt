package analysis.domain.powerset

import analysis.domain.{XMLDomain, Domain}
import analysis.domain.powerset.PowersetXMLDomain.{L, N}
import xml.{XMLComment, XMLAttribute, XMLTextNode, XMLNode}

import scala.collection.immutable.TreeSet

protected object OuterXPATH extends TypedPowersetXPathDomain[L]

/** This glues together the PowersetXMLDomain and the TypedPowersetXPathDomain and provides
  * the remaining method implementations.
  */
object TypedPowersetDomain extends Domain[N, L, OuterXPATH.V] {
  type V = TypedXPathValue[L]

  override val xmlDom = XML
  override val xpathDom = XPATH

  protected object XML extends PowersetXMLDomain.D[V] {
    override val xpathDom = XPATH

    override def createAttribute(name: String, value: V): N = value.str match {
      case None => None
      case Some(s) => Some(s.map(str => XMLAttribute(name, str)))
    }

    override def createTextNode(value: V): N = value.str match {
      case None => None
      case Some(s) => Some(s.collect {
        case str if str != "" => XMLTextNode(str)
      })
    }

    override def createComment(value: V): N = value.str match {
      case None => None
      case Some(s) => Some(s.collect {
        case str => XMLComment(str)
      })
    }
  }

  protected object XPATH extends OuterXPATH.D[N] {
    override val xmlDom: XMLDomain[N, L, OuterXPATH.V] = XML

    // Turn a node list into a set by sorting nodes in document order and removing duplicate nodes
    override def nodeListToSet(list: L): L = list match {
      case Left(_) => Left(None) // size might be different because duplicates are removed
      case Right(s) => Right(s.map(nodes => (TreeSet[XMLNode]() ++ nodes).toList))
    }
  }
}
