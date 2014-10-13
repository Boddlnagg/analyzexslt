package analysis.domain

import xml._
import xslt._

trait XMLDomain[N, L, V] {
  def top: N
  def bottom: N

  def listTop: L
  def listBottom: L

  def join(n1: N, n2: N): N
  //def meet(n1: N, n2: N): N
  //def compare(morePrecise: N, lessPrecise: N): Boolean

  def listJoin(l1: L, l2: L): L
  def listJoin(lists: List[L]): L = lists match {
    case Nil => listBottom
    case List(list) => list
    case _ => lists.reduceLeft(listJoin)
  }

  def liftDocument(root: XMLRoot): N

  // TODO: in order to support <xsl:element> this would need to take the name as V
  def liftElement(name: String, attributes: L, children: L): N

  def liftElement(name: String): N = liftElement(name, liftList(Nil), liftList(Nil))

  def liftAttribute(name: String, value: V): N

  def liftTextNode(value: V): N

  def liftList(nodes: List[N]): L

  def getRoot(node: N): N

  def getChildren(node: N): L

  def getParent(node: N): N

  def hasParent(node: N, parent: N): (N, N)

  def hasAncestor(node: N, ancestor: N): (N, N)

  def listConcat(list1: L, list2: L): L

  def partitionAttributes(list: L): (L, L)

  def wrapInRoot(list: L): N

  def copyToOutput(list: L): L // TODO: maybe use map() and copy() for single nodes instead?

  def flatMapWithIndex(list: L, f: (N, V) => L): L

  def getNodeListSize(list: L): V

  def getStringValue(node: N): V

  def isRoot(node: N): (N, N)

  def isElement(node: N): (N, N)

  def isTextNode(node: N): (N, N)

  def isComment(node: N): (N, N)

  def isAttribute(node: N): (N, N)

  def nameMatches(node: N, name: String): (N, N)

  // return empty string if node has no name
  def getNodeName(node: N): V
}