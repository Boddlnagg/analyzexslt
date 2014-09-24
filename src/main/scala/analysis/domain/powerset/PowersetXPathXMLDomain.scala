package analysis.domain.powerset

import analysis.domain.powerset.PowersetXMLDomain.N
import analysis.domain.powerset.PowersetXMLDomain.L
import analysis.domain.powerset.PowersetXPathDomain.T
import xpath._
import xml.{XMLAttribute, XMLTextNode, XMLNode}

import scala.collection.immutable.TreeSet

object PowersetXPathXMLDomain extends PowersetXPathDomain.D[N, L, PowersetXMLDomain.D.type] {
  val xmlDom = PowersetXMLDomain.D

  // TODO: replace this with toNodeSet(L), because we already have liftList(List[N])
  // but maybe this is not even needed
  override def liftNodeSet(set: Set[N]): T = {
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

  override def evaluateLocationPath(startNodeSet: T, steps: List[XPathStep], isAbsolute: Boolean): T = startNodeSet match {
    case None => None
    // TODO: implement this using join instead of flatMap
    case Some(values) => Some(values.flatMap {
      case nodes@NodeSetValue(_) => Set[XPathValue](NodeSetValue(XPathEvaluator.evaluateLocationPath(TreeSet[XMLNode]() ++ nodes.nodes, steps, isAbsolute).toList))
      case _ => Set[XPathValue]() // bottom
    })
  }

  override def getStringValue(node: N): T = node.map(_.map(n => StringValue(n.stringValue)))

  override def flatMapWithIndex(list: L, f: (N, T) => L): L = list match {
    case None => None
    case Some(s) => xmlDom.listJoin(s.map { l =>
      val mapped = l.zipWithIndex.map { case (n, i) => f(Some(Set(n)), Some(Set(NumberValue(i)))) }
      val flattened = mapped.foldLeft(xmlDom.liftList(Nil))((acc, next) => xmlDom.listConcat(acc, next))
      flattened
    })
  }

  // TODO: this could be generalized over XPath domains using `liftNumber` and `join` (but we don't have the T parameter in XMLDomain)
  override def getNodeListSize(list: L): T = list.map(_.map(l => NumberValue(l.size)))

  override def getConcatenatedTextNodeValues(list: L): T =
    list.map(_.map(l => StringValue(l.collect { case n: XMLTextNode => n.value }.mkString(""))))

  override def liftAttribute(name: String, value: T): N = value match {
    case None => None
    case Some(s) => Some(s.collect {
      case StringValue(str) => XMLAttribute(name, str)
      // NOTE: other XPath values are evaluated to bottom implicitly
    })
  }

}
