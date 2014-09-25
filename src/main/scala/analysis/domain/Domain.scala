package analysis.domain

trait Domain[N, L, V] {
  val xmlDom: XMLDomain[N, L, V]
  val xpathDom: XPathDomain[V, N, L]
}
