package analysis

import analysis.domain.{XPathDomain, XMLDomain}

// TODO: abstract over position/size abstract domain?
case class AbstractXPathContext[N, L, D1 <: XMLDomain[N, L], T, -D2 <: XPathDomain[T, N, L, D1]](node: N, position: Option[Int], size: Option[Int], variables: Map[String, T])