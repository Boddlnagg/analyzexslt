package analysis.domain.powerset

import analysis.domain.XPathDomain
import xml._
import xpath._

import scala.collection.immutable.TreeSet

/** Just a wrapper for the type alias */
object PowersetXPathDomain {
  type V = Option[Set[XPathValue]] // None represents the infinite set, Some represents finite sets

  /** This is the actual (partial) domain implementation */
  trait D[N, L] extends XPathDomain[V, N, L] {
    override def top: V = None
    override def bottom: V = BOT

    protected val BOT: V = Some(Set())

    // booleans are a finite domain so we don't need to represent an unknown boolean as None
    protected val anyBoolean: V = Some(Set(BooleanValue(true), BooleanValue(false)))

    override def join(v1: V, v2: V): V = (v1, v2) match {
      case (None, _) => None
      case (_, None) => None
      case (Some(s1), Some(s2)) => Some(s1.union(s2))
    }

    /*override def meet(v1: V, v2: V): V = (v1, v2) match {
      case (None, _) => None
      case (_, None) => None
      case (Some(s1), Some(s2)) => Some(s1.intersect(s2))
    }*/

    override def lessThanOrEqual(v1: V, v2: V): Boolean = (v1, v2) match {
      case (_, None) => true
      case (None, Some(_)) => false
      case (Some(s1), Some(s2)) => s1.subsetOf(s2)
    }

    override def topNumber: V = None // no type distinction in this domain

    override def topString: V = None // no type distinction in this domain

    protected def liftBinaryOp(left: V, right: V, pf: PartialFunction[(XPathValue, XPathValue), XPathValue]): V = (left, right) match {
      case (BOT, _) => BOT
      case (_, BOT) => BOT
      case (Some(s1), Some(s2)) => Some(s1.cross(s2).collect(pf).toSet)
      case _ => None
    }

    protected def liftBinaryLogicalOp(left: V, right: V, f: (XPathValue, XPathValue) => XPathValue): V =
      liftBinaryOp(left, right, { case (v1, v2) => f(v1, v2) }) match {
        case None => anyBoolean
        case Some(s) => Some(s)
      }

    protected def liftBinaryNumOp(left: V, right: V, f: (Double, Double) => Double): V = (left, right) match {
      case (BOT, _) => BOT
      case (_, BOT) => BOT
      case (Some(s1), Some(s2)) => Some(s1.cross(s2)
        .map { case (v1, v2) => NumberValue(f(v1.toNumberValue.value, v2.toNumberValue.value))}
        .toSet
      )
      case _ => None
    }

    override def add(left: V, right: V): V = liftBinaryNumOp(left, right,
      (v1, v2) => v1 + v2
    )

    override def subtract(left: V, right: V): V = liftBinaryNumOp(left, right,
      (v1, v2) => v1 - v2
    )

    override def multiply(left: V, right: V): V = liftBinaryNumOp(left, right,
      (v1, v2) => v1 * v2
    )

    override def divide(left: V, right: V): V = liftBinaryNumOp(left, right,
      (v1, v2) => v1 / v2
    )

    override def modulo(left: V, right: V): V = liftBinaryNumOp(left, right,
      (v1, v2) => v1 % v2
    )

    override def compareRelational(left: V, right: V, relOp: RelationalOperator): V = liftBinaryLogicalOp(left, right,
      (v1, v2) => BooleanValue(v1.compare(v2, relOp))
    )

    override def negateNum(v: V): V = v.map(_.map(num => NumberValue(-num.toNumberValue.value)))

    override def toStringValue(v: V): V = v.map(_.map(_.toStringValue))
    override def toNumberValue(v: V): V = v.map(_.map(_.toNumberValue))
    override def toBooleanValue(v: V): V = v.map(_.map(_.toBooleanValue))

    override def liftLiteral(lit: String): V = Some(Set(StringValue(lit)))

    override def liftNumber(num: Double): V = Some(Set(NumberValue(num)))

    override def liftBoolean(bool: Boolean): V = Some(Set(BooleanValue(bool)))

    override def nodeSetUnion(left: V, right: V): V = liftBinaryOp(left, right, {
      case (NodeSetValue(lVal), NodeSetValue(rVal)) => NodeSetValue((TreeSet[XMLNode]() ++ lVal ++ rVal).toList)
      // NOTE: ignore values that are not node-sets by not including them in the result (essentially evaluating them to bottom)
    })
  }
}
