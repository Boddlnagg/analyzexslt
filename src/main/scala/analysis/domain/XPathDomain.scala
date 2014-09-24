package analysis.domain

import xpath.{RelationalOperator, XPathStep}

trait XPathDomain[T, N, L, D1 <: XMLDomain[N, L]] {
  def top: T
  def bottom: T

  //def join(v1: T, v2: T): T
  //def meet(v1: T, v2: T): T

  def add(left: T, right: T): T
  def subtract(left: T, right: T): T
  def multiply(left: T, right: T): T
  def divide(left: T, right: T): T
  def modulo(left: T, right: T): T
  def compare(left: T, right: T, relOp: RelationalOperator): T
  def logicalAnd(left: T, right: T): T
  def logicalOr(left: T, right: T): T
  def negateNum(v: T): T
  def negateBool(v: T): T
  def liftLiteral(lit: String): T
  def liftNumber(num: Double): T
  def liftBoolean(bool: Boolean): T
  def liftNodeSet(set: Set[N]): T
  def nodeSetUnion(left: T, right: T): T
  def toStringValue(v: T): T
  def toNumberValue(v: T): T
  def toBooleanValue(v: T): T

  // TODO: implement this in an abstract way in the analyzer instead (not in each domain)
  def evaluateLocationPath(startNodeSet: T, steps: List[XPathStep], isAbsolute: Boolean): T

  def getStringValue(node: N): T
  def flatMapWithIndex(list: L, f: (N, T) => L): L
  def getNodeListSize(list: L): T
  def getConcatenatedTextNodeValues(list: L): T
  def liftAttribute(name: String, value: T): N
 }