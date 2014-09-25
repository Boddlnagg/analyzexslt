package analysis.domain

import xml._
import xslt._

trait XMLDomain[N, L] {
  def top: N
  def bottom: N

  def listTop: L
  def listBottom: L

  def map(list: L, f: N => N): L

  def join(n1: N, n2: N): N
  //def meet(n1: N, n2: N): N
  //def compare(morePrecise: N, lessPrecise: N): Boolean

  def listJoin(l1: L, l2: L): L
  def listJoin(lists: Traversable[L]): L = lists.reduceLeft(listJoin)

  def chooseTemplates(sheet: XSLTStylesheet, n: N): Map[XSLTTemplate, N]

  def lift(n: XMLNode): N

  def liftList(nodes: List[N]): L

  def getRoot(node: N): N

  def getChildren(node: N): L

  def listConcat(list1: L, list2: L): L

  def appendChildren(node: N, list: L): N

  def addAttributes(node: N, list: L): N

  def partitionAttributes(list: L): (L, L)

  def wrapInRoot(list: L): N
}