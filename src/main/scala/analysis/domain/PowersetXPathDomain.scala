package analysis.domain

import analysis.AbstractXPathContext
import util.EvaluationError
import xml._
import xpath._

import scala.collection.immutable.TreeSet

trait PowersetXPathDomain[N, D1 <: XMLDomain[N]] {
  type T = Option[Set[XPathValue]] // None represents the infinite set, Some represents finite sets

  object D extends XPathDomain[T, N, D1] {
    override def top: T = Some(Set())
    override def bottom: T = None

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
        .map { case (v1, v2) => (v1.toNumberValue.value, v2.toNumberValue.value) }
        .collect(pf)
        .map(NumberValue(_))
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
    override def negate(v: T): T = v.map(_.map(num => NumberValue(- num.toNumberValue.value)))
    override def liftLiteral(lit: String): T = Some(Set(StringValue(lit)))
    override def liftNumber(num: Double): T = Some(Set(NumberValue(num)))

    override def liftNodeSet(set: Set[N]): T = ??? // TODO
    
    override def nodeSetUnion(left: T, right: T): T = liftBinaryOp(left, right, {
      case (NodeSetValue(lVal), NodeSetValue(rVal)) => NodeSetValue((TreeSet[XMLNode]()++ lVal ++ rVal).toList)
      // NOTE: ignore values that are not node-sets by not including them in the result (essentially evaluating them to bottom)
    })

    def evalFunction(name: String, params: List[T], ctx: AbstractXPathContext[N,D1,T,PowersetXPathDomain.this.D.type]): T = (name, params) match {
      // TODO: some of these functions are the same for each domain (given a generic lift operator)
      case ("true", Nil) => Some(Set(BooleanValue(true)))
      case ("false", Nil) => Some(Set(BooleanValue(false)))
      case ("not", List(arg)) => arg match {
        case None => anyBoolean
        case Some(s) => Some(s.map(b => BooleanValue(!b.toBooleanValue.value)))
      }
      case ("string", List(arg)) => arg.map(_.map(_.toStringValue))
      case ("boolean", List(arg)) => arg.map(_.map(_.toBooleanValue))
      case ("number", List(arg)) => arg.map(_.map(_.toNumberValue))
      case ("last", Nil) => ctx.size.map(s => Set(NumberValue(s)))
      case ("position", Nil) => ctx.position.map(p => Set(NumberValue(p)))
      // TODO: implement these functions?
      /*case ("count", List(NodeSetValue(nodes))) => NumberValue(nodes.size)
      case ("sum", List(NodeSetValue(nodes))) => NumberValue(nodes.map(n => StringValue(n.stringValue).toNumberValue.value).sum)
      case ("name"|"local-name", List(NodeSetValue(List(node)))) => node match {
        case XMLElement(nodeName, _, _, _) => StringValue(nodeName)
        case XMLAttribute(nodeName, _, _) => StringValue(nodeName)
        case _ => StringValue("")
      }*/
      case (_, evaluatedParams) =>
        throw new EvaluationError(f"Unknown function '$name' (might not be implemented) or invalid number/types of parameters ($evaluatedParams).")
    }

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
