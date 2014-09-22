package analysis.domain.powerset

import analysis.domain.powerset.PowersetXPathDomain.T
import xpath.{XPathValue, XPathStep, NodeSetValue, XPathEvaluator}
import xml.XMLNode

import scala.collection.immutable.TreeSet

object PowersetXPathXMLDomain extends PowersetXPathDomain.D[PowersetXMLDomain.N, PowersetXMLDomain.D.type] {
  // TODO: is this needed? can this be abstracted over the XML domain type?
  override def liftNodeSet(set: Set[PowersetXMLDomain.N]): T = {
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
      case nodes@NodeSetValue(_) => Set[XPathValue](XPathEvaluator.evaluateLocationPath(nodes, steps, isAbsolute))
      case _ => Set[XPathValue]() // bottom
    })
  }
}
