package analysis.domain

/** A domain for abstract interpretation of XSLT and XPath.
  * @tparam N The type of abstract XML nodes.
  * @tparam L The type of abstract XML node lists.
  * @tparam V The type of abstract XPath values.
  */
trait Domain[N, L, V] {
  /** The XML domain for this domain, providing operations on XML nodes (N) and list of nodes (L). */
  val xmlDom: XMLDomain[N, L, V]
  /** The XPath domain for this domain, providing operations on XPath values (V). */
  val xpathDom: XPathDomain[V, N, L]
}
