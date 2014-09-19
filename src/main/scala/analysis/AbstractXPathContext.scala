package analysis

import analysis.domain.{XPathDomain, XMLDomain}

// TODO: abstract over position/size abstract domain?
case class AbstractXPathContext[N, D1 <: XMLDomain[N], T, -D2 <: XPathDomain[T]](node: N, position: Option[Int], size: Option[Int], variables: Map[String, T])