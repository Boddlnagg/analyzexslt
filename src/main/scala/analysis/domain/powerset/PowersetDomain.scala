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
        case StringValue(str) => XMLTextNode(str)
        // NOTE: other value types are implicitly evaluated to bottom
      })
    }

    /** Appends text to a node list.
      * Empty strings are ignored (return the unmodified list) and text that immediately follows
      * an existing text node is merged into that text node.
      */
    override def appendText(list: L, text: V): L = {
      def appendTextInternal(list: List[XMLNode], text: String): List[XMLNode] = {
        list match {
          case Nil => List(XMLTextNode(text))
          case XMLTextNode(oldValue, parent) :: Nil => List(XMLTextNode(oldValue + text, parent))
          case last :: Nil => List(last, XMLTextNode(text))
          case first :: rest => first :: appendTextInternal(rest, text)
        }
      }

      val textStrings = text.map(_.collect {
        case StringValue(str) => str
      })

      if (textStrings == Some(Set()) || list == Right(Set())) { // text or list is BOTTOM
        Right(Set())
      } else if (textStrings == Some(Set(""))) {
        list // return original list if empty string is appended
      } else if (textStrings == None) {
        Left(None) // don't know result list if text to append is TOP
      } else {
        val filteredText = textStrings.get.filter(s => s != "")
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
