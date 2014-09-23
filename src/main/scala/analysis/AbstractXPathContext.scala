package analysis

import analysis.domain.{XPathDomain, XMLDomain}

case class AbstractXPathContext[N, L, D1 <: XMLDomain[N, L], T, -D2 <: XPathDomain[T, N, L, D1]](
  node: N,
  position: T,
  size: T,
  variables: Map[String, T]
)