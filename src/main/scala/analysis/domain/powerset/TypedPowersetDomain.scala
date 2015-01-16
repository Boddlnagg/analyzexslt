package analysis.domain.powerset

import analysis.domain.{XMLDomain, Domain}
import analysis.domain.powerset.PowersetXMLDomain.{L, N}
import xml._

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

    override def createElement(name: V, attributes: L, children: L): N = (name.str, attributes, children) match {
      case (Some(s), _, _) if s.isEmpty => BOT
      case (_, Right(s), _) if s.isEmpty => BOT
      case (_, _, Right(s)) if s.isEmpty => BOT
      case (None, _, _) => None
      case (_, Left(_), _) => None
      case (_, _, Left(_)) => None
      case (Some(s1), Right(s2), Right(s3)) => Some(s1.cross(s2).cross(s3).collect {
        case ((n, attr), chld) => XMLElement(n,
          attr.map(a => a.asInstanceOf[XMLAttribute].copy),
          chld.map(c => c.copy))
      }.toSet)
    }

    override def createAttribute(name: V, value: V): N = (name.str, value.str) match {
      case (Some(s), _) if s.isEmpty => BOT
      case (_, Some(s)) if s.isEmpty => BOT
      case (None, _) | (_, None) => None
      case (Some(s1), Some(s2)) => Some(s1.cross(s2).map { case (n, v) => XMLAttribute(n, v)}.toSet)
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
      case Right(s) => Right(s.map(nodes => nodes.to[TreeSet].toList)) // convert to ordered set and back to list
    }
  }
}
