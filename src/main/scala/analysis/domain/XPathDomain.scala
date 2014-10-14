package analysis.domain

import xpath.{RelationalOperator, XPathStep}

trait XPathDomain[V, N, L] {
  def top: V
  def bottom: V

  def join(v1: V, v2: V): V
  //def meet(v1: V, v2: V): V

  def join(values: List[V]): V = values match {
    case Nil => bottom
    case List(v) => v
    case _ => values.reduceLeft(join)
  }

  def add(left: V, right: V): V
  def subtract(left: V, right: V): V
  def multiply(left: V, right: V): V
  def divide(left: V, right: V): V
  def modulo(left: V, right: V): V
  def compare(left: V, right: V, relOp: RelationalOperator): V
  def logicalAnd(left: V, right: V): V
  def logicalOr(left: V, right: V): V
  def negateNum(v: V): V
  def negateBool(v: V): V
  def liftLiteral(lit: String): V
  def liftNumber(num: Double): V
  def liftBoolean(bool: Boolean): V

  // converts a list to a node-set value (orders in document order, removes duplicates, wraps in XPath value)
  def toNodeSet(list: L): V
  def nodeSetUnion(left: V, right: V): V
  def toStringValue(v: V): V
  def toNumberValue(v: V): V
  def toBooleanValue(v: V): V
  def maybeTrue(v: V): Boolean
  def maybeFalse(v: V): Boolean
  def matchNodeSetValues(value: V): (L, V)
 }