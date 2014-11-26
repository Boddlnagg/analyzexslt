package analysis.domain.zipper

import analysis.domain.Domain
import analysis.domain.zipper.ZipperXMLDomain.L
import analysis.domain.powerset.{TypedXPathValue, TypedPowersetXPathDomain}
import analysis.domain.zipper.ZipperXMLDomain.N

protected object OuterXPATH extends TypedPowersetXPathDomain[L]

object ZipperDomain extends Domain[N, L, OuterXPATH.V] {
  type V = TypedXPathValue[L]

  override val xmlDom = XML
  override val xpathDom = XPATH

  protected object XML extends ZipperXMLDomain.D[V] {
    override val xpathDom = XPATH
  }

  protected object XPATH extends OuterXPATH.D[N] {
    override val xmlDom = XML

    /** A node-set is converted to a string by returning the string-value of the node in the node-set that is
      * first in document order. If the node-set is empty, an empty string is returned.
      */
    override def nodeSetToStringValue(nodeSet: L): Option[Set[String]] = ???

    /** Turn a node list into a set by sorting nodes in document order and removing duplicate nodes */
    override def nodeListToSet(list: L): L = ???
  }
}
