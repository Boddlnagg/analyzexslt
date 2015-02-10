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
  type V = (Set[Boolean], Option[Set[Double]], Option[Set[String]], L)

  override val xmlDom = XML
  override val xpathDom = XPATH

  protected object XML extends PowersetXMLDomain.D[V] {
    override val xpathDom = XPATH

    /** Create an element node with the given name, attributes and children.
      * The output is created bottom-up, so children are always created before their parent nodes.
      * Consecutive text node children must be merged into a single text node by this method.
      */
    override def createElement(name: V, attributes: L, children: L): N = (name._3, attributes, children) match {
      case (Some(s), _, _) if s.isEmpty => BOT
      case (_, Some(s), _) if s.isEmpty => BOT
      case (_, _, Some(s)) if s.isEmpty => BOT
      case (None, _, _) => None
      case (_, None, _) => None
      case (_, _, None) => None
      case (Some(s1), Some(s2), Some(s3)) => Some(s1.cross(s2).cross(s3).collect {
        case ((n, attr), chld) => XMLElement(n,
          attr.map(a => a.asInstanceOf[XMLAttribute].copy),
          chld.map(c => c.copy))
      }.toSet)
    }

    /** Create an attribute node with the given name and text value.
      * Values that are not strings evaluate to BOTTOM.
      */
    override def createAttribute(name: V, value: V): N = (name._3, value._3) match {
      case (Some(s), _) if s.isEmpty => BOT
      case (_, Some(s)) if s.isEmpty => BOT
      case (None, _) | (_, None) => None
      case (Some(s1), Some(s2)) => Some(s1.cross(s2).map { case (n, v) => XMLAttribute(n, v)}.toSet)
    }

    /** Create a text node with the given text value. Values that are not strings evaluate to BOTTOM.
      * The empty string also evaluates to BOTTOM, because text nodes with no content are not allowed.
      */
    override def createTextNode(value: V): N = value._3 match {
      case None => None
      case Some(s) => Some(s.collect {
        case str if str != "" => XMLTextNode(str)
      })
    }

    /** Create a comment node with the given text value. Values that are not strings evaluate to BOTTOM. */
    override def createComment(value: V): N = value._3 match {
      case None => None
      case Some(s) => Some(s.collect {
        case str => XMLComment(str)
      })
    }
  }

  protected object XPATH extends OuterXPATH.D[N] {
    override val xmlDom: XMLDomain[N, L, OuterXPATH.V] = XML

    /** Turn a node list into a set by sorting nodes in document order and removing duplicate nodes */
    override def nodeListToSet(list: L): L = list match {
      case None => None // size might be different because duplicates are removed
      case Some(s) => Some(s.map(nodes => nodes.to[TreeSet].toList)) // convert to ordered set and back to list
    }
  }
}
