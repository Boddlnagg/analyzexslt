case class XPathContext(node: XMLNode, position: Int, size: Int, variables: Map[String, XPathValue])

class EvaluationException(message: String = null, cause: Throwable = null) extends RuntimeException(message, cause)

object XPathEvaluator {
  def evaluate(expr: XPathExpr, ctx: XPathContext): XPathValue = {
    expr match {
      case PlusExpr(lhs, rhs) => NumberValue(evaluate(lhs, ctx).toNumberValue.value + evaluate(rhs, ctx).toNumberValue.value)
      case MinusExpr(lhs, rhs) => NumberValue(evaluate(lhs, ctx).toNumberValue.value - evaluate(rhs, ctx).toNumberValue.value)
      case MultiplyExpr(lhs, rhs) => NumberValue(evaluate(lhs, ctx).toNumberValue.value * evaluate(rhs, ctx).toNumberValue.value)
      case DivExpr(lhs, rhs) => NumberValue(evaluate(lhs, ctx).toNumberValue.value / evaluate(rhs, ctx).toNumberValue.value)
      case ModExpr(lhs, rhs) => NumberValue(evaluate(lhs, ctx).toNumberValue.value % evaluate(rhs, ctx).toNumberValue.value)
      case RelationalExpr(lhs, rhs, relOp) =>
        // evaluation is specified in the XPath spec section 3.4
        val lhsVal = evaluate(lhs, ctx)
        val rhsVal = evaluate(rhs, ctx)
        // comparing node sets is especially tricky and therefore skipped in this implementation
        assert(!lhsVal.isInstanceOf[NodeSetValue] && !rhsVal.isInstanceOf[NodeSetValue], "Comparing node sets is not implemented")
        val result = relOp match {

          case EqualsOperator | NotEqualsOperator =>
            // result depends on types of values
            (lhsVal, rhsVal) match {
              // at least one operand is a boolean value -> compare both as booleans
              case (_, BooleanValue(_)) | (BooleanValue(_), _) => relOp match {
                case EqualsOperator => lhsVal.toBooleanValue.value == rhsVal.toBooleanValue.value
                case NotEqualsOperator => lhsVal.toBooleanValue.value != rhsVal.toBooleanValue.value
              }
              // at least one operand is a number -> compare both as numbers
              case (_, NumberValue(_)) | (NumberValue(_), _) => relOp match {
                case EqualsOperator => lhsVal.toNumberValue.value == rhsVal.toNumberValue.value
                case NotEqualsOperator => lhsVal.toNumberValue.value != rhsVal.toNumberValue.value
              }
              // otherwise compare both as strings
              case _ => relOp match {
                case EqualsOperator => lhsVal.toStringValue.value == rhsVal.toStringValue.value
                case NotEqualsOperator => lhsVal.toStringValue.value != rhsVal.toStringValue.value
              }
            }
          case LessThanOperator => lhsVal.toNumberValue.value < rhsVal.toNumberValue.value
          case GreaterThanOperator => lhsVal.toNumberValue.value > rhsVal.toNumberValue.value
          case LessThanEqualOperator => lhsVal.toNumberValue.value <= rhsVal.toNumberValue.value
          case GreaterThanEqualOperator => lhsVal.toNumberValue.value >= rhsVal.toNumberValue.value
        }
        BooleanValue(result)
      // XPath spec section 3.4, shortcut evaluation!
      case AndExpr(lhs, rhs) => BooleanValue(evaluate(lhs, ctx).toBooleanValue.value && evaluate(rhs, ctx).toBooleanValue.value)
      // XPath spec section 3.4, shortcut evaluation!
      case OrExpr(lhs, rhs) => BooleanValue(evaluate(lhs, ctx).toBooleanValue.value || evaluate(rhs, ctx).toBooleanValue.value)

      case NegExpr(subexpr) => NumberValue(- evaluate(subexpr, ctx).toNumberValue.value)
      case LiteralExpr(literal) => StringValue(literal)
      case NumberExpr(num) => NumberValue(num)
      case VariableReferenceExpr(name) => ctx.variables(name)
      case UnionExpr(lhs, rhs) => (evaluate(lhs, ctx), evaluate(rhs, ctx)) match {
        case (NodeSetValue(left), NodeSetValue(right)) => ???
        case (left, right) => throw new EvaluationException(f"Wrong types for union expression, must be node-sets ($left | $right)")
      }
      case FunctionCallExpr(name, params) =>
        // See XPath spec section 3.2
        (name, params.map(p => evaluate(p, ctx))) match {
          // arguments are casted to string, number, boolean as required, but if a function expects a node-set, it must be a node-set
          case ("true", Nil) => BooleanValue(true)
          case ("false", Nil) => BooleanValue(false)
          case ("not", List(arg)) => BooleanValue(!arg.toBooleanValue.value)
          case ("string", List(arg)) => arg.toStringValue
          case ("boolean", List(arg)) => arg.toBooleanValue
          case ("number", List(arg)) => arg.toNumberValue
          case (_, evaluatedParams) =>
            throw new EvaluationException(f"Unknown function '$name' (might not be implemented) or invalid number/types of parameters ($evaluatedParams).")
        }
      case LocationPath(steps, isAbsolute) => ??? // TODO: evaluate steps from left to right, keep nodes in document order if possible
      case PathExpr(filter, locationPath) => ???
      case FilterExpr(expr, predicates) => ???
    }
  }
}
