package analysis.domain

import analysis.AbstractXPathContext
import xpath._

trait XPathDomain[T] {
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
  def nodeSetUnion(left: T, right: T): T
  def evalFunction[N, D1 <: XMLDomain[N]](name: String, params: List[T], ctx: AbstractXPathContext[N,D1,T,this.type]): T

  def join(v1: T, v2: T): T
  def meet(v1: T, v2: T): T
 }