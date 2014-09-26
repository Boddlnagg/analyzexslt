package analysis.domain.powerset

import analysis.domain.Domain
import analysis.domain.powerset.PowersetXMLDomain.N
import analysis.domain.powerset.PowersetXMLDomain.L
import analysis.domain.powerset.PowersetXPathDomain.V
import xpath._
import xml.{XMLAttribute, XMLTextNode, XMLNode}

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

    // TODO: replace this with toNodeSet(L), because we already have liftList(List[N])
    // but maybe this is not even needed
    override def liftNodeSet(set: Set[N]): V = {
      def getProduct(input:List[List[XMLNode]]): List[List[XMLNode]] = input match{
        case Nil => Nil // just in case you input an empty list
        case head::Nil => head.map(_::Nil)
        case head::tail => for(elem<- head; sub <- getProduct(tail)) yield elem::sub
      }

      if (set.exists(n => !n.isDefined))
        None
      else
        Some(getProduct(set.map(_.get.toList).toList).map(nodes => NodeSetValue((TreeSet[XMLNode]() ++ nodes).toList)).toSet)
    }

    override def evaluateLocationPath(startNodeSet: V, steps: List[XPathStep], isAbsolute: Boolean): V = startNodeSet match {
      case None => None
      // TODO: implement this using join instead of flatMap
      case Some(values) => Some(values.flatMap {
        case nodes@NodeSetValue(_) => Set[XPathValue](NodeSetValue(XPathEvaluator.evaluateLocationPath(TreeSet[XMLNode]() ++ nodes.nodes, steps, isAbsolute).toList))
        case _ => Set[XPathValue]() // bottom
      })
    }

    override def getConcatenatedTextNodeValues(list: L): V =
      list.map(_.map(l => StringValue(l.collect { case n: XMLTextNode => n.value }.mkString(""))))



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
