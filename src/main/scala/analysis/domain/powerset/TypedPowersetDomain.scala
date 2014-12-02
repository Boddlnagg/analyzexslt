package analysis.domain.powerset

import analysis.domain.{XMLDomain, Domain}
import analysis.domain.powerset.PowersetXMLDomain.{L, N}
import xml.{XMLAttribute, XMLTextNode, XMLNode}

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
      case Some(s) => Some(s.map(str => XMLTextNode(str)))
    }

    override def appendText(list: L, text: V): L = {
      def appendTextInternal(list: List[XMLNode], text: String): List[XMLNode] = {
        list match {
          case Nil => List(XMLTextNode(text))
          case XMLTextNode(oldValue, parent) :: Nil => List(XMLTextNode(oldValue + text, parent))
          case last :: Nil => List(last, XMLTextNode(text))
          case first :: rest => first :: appendTextInternal(rest, text)
        }
      }

      if (text.str == Some(Set()) || list == Right(Set())) { // text or list is BOTTOM
        Right(Set())
      } else if (text.str == Some(Set(""))) {
        list // return original list if empty string is appended
      } else if (text.str == None) {
        Left(None) // don't know result list if text to append is TOP
      } else {
        val filteredText = text.str.get.filter(s => s != "")
        list match {
          case Left(None) => Left(None)
          case Left(Some(len)) => Left(None) // length might be len or len+1
          case Right(s) => Right(s.cross(filteredText).map {
            case (l, t) => appendTextInternal(l, t)
          }.toSet)
        }
      }
    }
  }

  protected object XPATH extends OuterXPATH.D[N] {
    override val xmlDom: XMLDomain[N, L, OuterXPATH.V] = XML

    // A node-set is converted to a string by returning the string-value of the node in the node-set that is first in document order.
    override def nodeSetToStringValue(nodeSet: L): Option[Set[String]] = nodeSet match {
      case Left(_) => None
      case Right(s) => Some(s.map {
        case Nil => ""
        case List(node) => node.stringValue
        case _ => return None // Converting node sets to strings is not implemented for node-sets with more than one element
      })
    }

    // Turn a node list into a set by sorting nodes in document order and removing duplicate nodes
    override def nodeListToSet(list: L): L = list match {
      case Left(_) => Left(None) // size might be different because duplicates are removed
      case Right(s) => Right(s.map(nodes => (TreeSet[XMLNode]() ++ nodes).toList))
    }
  }
}
