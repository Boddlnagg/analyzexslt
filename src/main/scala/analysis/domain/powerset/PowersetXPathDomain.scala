package analysis.domain.powerset

import analysis.AbstractXPathContext
import analysis.domain.{XMLDomain, XPathDomain}
import util.EvaluationError
import xml._
import xpath._

import scala.collection.immutable.TreeSet

object PowersetXPathDomain {
  type T = Option[Set[XPathValue]] // None represents the infinite set, Some represents finite sets

  trait D[N, L, XD <: XMLDomain[N, L]] extends XPathDomain[T, N, L, XD] {
    override def top: T = None
    override def bottom: T = Some(Set())

    // booleans are a finite domain so we don't need to represent an unknown boolean as None
    val anyBoolean: T = Some(Set(BooleanValue(true), BooleanValue(false)))

    def liftBinaryOp(left: T, right: T, pf: PartialFunction[(XPathValue, XPathValue), XPathValue]): T = (left, right) match {
      case (None, _) => None
      case (_, None) => None
      case (Some(s1), Some(s2)) => Some(s1.cross(s2).collect(pf).toSet)
    }

    def liftBinaryLogicalOp(left: T, right: T, pf: PartialFunction[(XPathValue, XPathValue), XPathValue]): T =
      liftBinaryOp(left, right, pf) match {
        case None => anyBoolean
        case Some(s) => Some(s)
      }

    def liftBinaryNumOp(left: T, right: T, pf: PartialFunction[(Double, Double), Double]): T = (left, right) match {
      case (None, _) => None
      case (_, None) => None
      case (Some(s1), Some(s2)) => Some(s1.cross(s2)
        .map { case (v1, v2) => (v1.toNumberValue.value, v2.toNumberValue.value)}
        .collect(pf)
        .map(NumberValue)
        .toSet
      )
    }

    override def add(left: T, right: T): T = liftBinaryNumOp(left, right, {
      case (v1, v2) => v1 + v2
    })

    override def subtract(left: T, right: T): T = liftBinaryNumOp(left, right, {
      case (v1, v2) => v1 - v2
    })

    override def multiply(left: T, right: T): T = liftBinaryNumOp(left, right, {
      case (v1, v2) => v1 * v2
    })

    override def divide(left: T, right: T): T = liftBinaryNumOp(left, right, {
      case (v1, v2) if v2 != 0 => v1 / v2
    })

    override def modulo(left: T, right: T): T = liftBinaryNumOp(left, right, {
      case (v1, v2) if v2 != 0 => v1 % v2
    })

    override def compare(left: T, right: T, relOp: RelationalOperator): T = liftBinaryLogicalOp(left, right, {
      case (v1, v2) => BooleanValue(v1.compare(v2, relOp))
    })

    override def logicalAnd(left: T, right: T): T = liftBinaryLogicalOp(left, right, {
      // TODO: does shortcut evaluation matter in XPath? (it should use shortcut evaluation according to the spec)
      case (v1, v2) => BooleanValue(v1.toBooleanValue.value && v2.toBooleanValue.value)
    })

    override def logicalOr(left: T, right: T): T = liftBinaryLogicalOp(left, right, {
      // TODO: does shortcut evaluation matter in XPath? (it should use shortcut evaluation according to the spec)
      case (v1, v2) => BooleanValue(v1.toBooleanValue.value || v2.toBooleanValue.value)
    })

    override def negateNum(v: T): T = v.map(_.map(num => NumberValue(-num.toNumberValue.value)))

    override def negateBool(v: T): T = v match {
      case None => anyBoolean
      case Some(s) => Some(s.map(b => BooleanValue(!b.toBooleanValue.value)))
    }

    override def toStringValue(v: T): T = v.map(_.map(_.toStringValue))
    override def toNumberValue(v: T): T = v.map(_.map(_.toNumberValue))
    override def toBooleanValue(v: T): T = v.map(_.map(_.toBooleanValue))

    override def liftLiteral(lit: String): T = Some(Set(StringValue(lit)))

    override def liftNumber(num: Double): T = Some(Set(NumberValue(num)))

    override def liftBoolean(bool: Boolean): T = Some(Set(BooleanValue(bool)))

    override def nodeSetUnion(left: T, right: T): T = liftBinaryOp(left, right, {
      case (NodeSetValue(lVal), NodeSetValue(rVal)) => NodeSetValue((TreeSet[XMLNode]() ++ lVal ++ rVal).toList)
      // NOTE: ignore values that are not node-sets by not including them in the result (essentially evaluating them to bottom)
    })

    override def join(v1: T, v2: T): T = (v1, v2) match {
      case (None, _) => None
      case (_, None) => None
      case (Some(s1), Some(s2)) => Some(s1.union(s2))
    }

    override def meet(v1: T, v2: T): T = (v1, v2) match {
      case (None, _) => None
      case (_, None) => None
      case (Some(s1), Some(s2)) => Some(s1.intersect(s2))
    }
  }
}
