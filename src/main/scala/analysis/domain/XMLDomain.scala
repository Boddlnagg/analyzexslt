package analysis.domain

import xml._
import xslt._

trait XMLDomain[N] {
  def top: N
  def bottom: N
  def compare(morePrecise: N, lessPrecise: N): Boolean

  def chooseTemplates(sheet: XSLTStylesheet, n: N): Set[XSLTTemplate]
  
  def join(n1: N, n2: N): N
  def meet(n1: N, n2: N): N

  def lift(n: XMLNode): N

  def getRoot(node: N): N
}