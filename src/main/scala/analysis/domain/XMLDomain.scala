package analysis.domain

import xml._
import xslt._

trait XMLDomain[N, L] {
  def top: N
  def bottom: N

  def listTop: L
  def listBottom: L

  // TODO: provide a map operation for L (taking a function N -> N)

  def compare(morePrecise: N, lessPrecise: N): Boolean

  def chooseTemplates(sheet: XSLTStylesheet, n: N): Set[XSLTTemplate]
  
  def join(n1: N, n2: N): N
  def meet(n1: N, n2: N): N

  def lift(n: XMLNode): N

  def liftToList(n: N): L

  def getRoot(node: N): N

  def getChildren(node: N): L

  def listConcat(list1: L, list2: L): L

  def appendChildren(node: N, list: L): N

  def addAttributes(node: N, list: L): N
}