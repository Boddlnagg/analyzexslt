package analysis

import analysis.domain.{XPathDomain, XMLDomain}
import util.EvaluationError
import xml._
import xpath._

import scala.collection.immutable.TreeSet

trait XPathAnalyzer[N, D1 <: XMLDomain[N], T, D2 <: XPathDomain[T, N, D1]] {
  val dom1: D1
  val dom2: D2

  /** Evaluates a given XPath expression using a specified context and returns the result of the evaluation. */
  def evaluate(expr: XPathExpr, ctx: AbstractXPathContext[N, D1, T, D2]): T = {
    expr match {
      case PlusExpr(lhs, rhs) => dom2.add(evaluate(lhs, ctx), evaluate(rhs, ctx))
      case MinusExpr(lhs, rhs) => dom2.subtract(evaluate(lhs, ctx), evaluate(rhs, ctx))
      case MultiplyExpr(lhs, rhs) => dom2.multiply(evaluate(lhs, ctx), evaluate(rhs, ctx))
      case DivExpr(lhs, rhs) => dom2.divide(evaluate(lhs, ctx), evaluate(rhs, ctx))
      case ModExpr(lhs, rhs) => dom2.modulo(evaluate(lhs, ctx), evaluate(rhs, ctx))
      case RelationalExpr(lhs, rhs, relOp) => dom2.compare(evaluate(lhs, ctx), evaluate(rhs, ctx), relOp)
      case AndExpr(lhs, rhs) => dom2.logicalAnd(evaluate(lhs, ctx), evaluate(rhs, ctx))
      case OrExpr(lhs, rhs) => dom2.logicalOr(evaluate(lhs, ctx), evaluate(rhs, ctx))
      case NegExpr(subexpr) => dom2.negate(evaluate(subexpr, ctx))
      case LiteralExpr(literal) => dom2.liftLiteral(literal)
      case NumberExpr(num) => dom2.liftNumber(num)
      case VariableReferenceExpr(name) => try ctx.variables(name) catch {
        // because of static scoping this is an error in the program (no matter what evaluation strategy is used)
        case e: java.util.NoSuchElementException => throw new EvaluationError(f"Variable $name is not defined")
      }
      case UnionExpr(lhs, rhs) => dom2.nodeSetUnion(evaluate(lhs, ctx), evaluate(rhs, ctx))
      case FunctionCallExpr(name, params) => dom2.evalFunction(name, params.map(p => evaluate(p, ctx)), ctx)
      case _ => ??? // TODO
      /*case LocationPath(steps, isAbsolute) => NodeSetValue(evaluateLocationPath(ctx.node, steps, isAbsolute).toList)
      case PathExpr(filter, locationPath) =>
        evaluate(filter, ctx) match {
          case NodeSetValue(nodes) => NodeSetValue(nodes.flatMap {
            n => evaluateLocationPath(n, locationPath.steps, locationPath.isAbsolute).toList
          })
          case value => throw new EvaluationError(f"Filter expression must return a node-set (returned: $value)")
        }
      case FilterExpr(subexpr, predicates) =>
        if (!predicates.isEmpty) throw new NotImplementedError("Predicates are not supported")
        evaluate(subexpr, ctx)
      */
    }
  }
}
