package analysis.domain

import analysis.AbstractXPathContext
import xpath.{RelationalOperator, XPathStep}

trait XPathDomain[T, N, D1 <: XMLDomain[N]] {
  def top: T
  def bottom: T

  def add(left: T, right: T): T
  def subtract(left: T, right: T): T
  def multiply(left: T, right: T): T
  def divide(left: T, right: T): T
  def modulo(left: T, right: T): T
  def compare(left: T, right: T, relOp: RelationalOperator): T
  def logicalAnd(left: T, right: T): T
  def logicalOr(left: T, right: T): T
  def negate(v: T): T
  def liftLiteral(lit: String): T
  def liftNumber(num: Double): T
  def liftNodeSet(set: Set[N]): T
  def nodeSetUnion(left: T, right: T): T
  def evaluateFunction(name: String, params: List[T], ctx: AbstractXPathContext[N,D1,T,this.type]): T

  // TODO: implement this in an abstract way in the analyzer instead (not in each domain)
  def evaluateLocationPath(startNodeSet: T, steps: List[XPathStep], isAbsolute: Boolean): T

  def join(v1: T, v2: T): T
  def meet(v1: T, v2: T): T
 }