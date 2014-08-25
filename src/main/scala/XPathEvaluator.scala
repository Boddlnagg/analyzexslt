import scala.collection.immutable.TreeSet

case class XPathContext(node: XMLNode, position: Int, size: Int, variables: Map[String, XPathValue])

class EvaluationError(message: String = null, cause: Throwable = null) extends RuntimeException(message, cause)

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
        case (left, right) => throw new EvaluationError(f"Wrong types for union expression, must be node-sets ($left | $right)")
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
          case ("last", Nil) => NumberValue(ctx.size)
          case ("position", Nil) => NumberValue(ctx.position)
          case ("count", List(NodeSetValue(nodes))) => NumberValue(nodes.size)
          case (_, evaluatedParams) =>
            throw new EvaluationError(f"Unknown function '$name' (might not be implemented) or invalid number/types of parameters ($evaluatedParams).")
        }
      case LocationPath(steps, isAbsolute) => NodeSetValue(evaluateLocationPath(ctx.node, steps, isAbsolute, ctx.variables).toList)
      case PathExpr(filter, locationPath) => ???
      case FilterExpr(expr, predicates) => ???
    }
  }

  private def evaluateLocationPath(ctxNode: XMLNode, steps: List[XPathStep], isAbsolute: Boolean, variables: Map[String, XPathValue]): TreeSet[XMLNode] = {
    // evaluate steps from left to right, keep nodes in document order (not required by XPath, but by XSLT)
    (steps, isAbsolute) match {
      case (Nil, true) => TreeSet(ctxNode.root)
      case (steps, true) => evaluateLocationPath(ctxNode.root, steps, false, variables)
      case (first :: rest, false) =>
        val nodes: TreeSet[XMLNode] = first.axis match {
          // the child axis contains the children of the context node
          case ChildAxis => ctxNode match {
            case XMLRoot(elem) => TreeSet(elem)
            case XMLElement(_, _, children, _) => TreeSet[XMLNode]() ++ children
            case _ => TreeSet()
          }
          // the descendant axis contains the descendants of the context node
          // a descendant is a child or a child of a child and so on
          case DescendantAxis => TreeSet[XMLNode]() ++ ctxNode.descendants
          // the parent axis contains the parent of the context node, if there is one
          case ParentAxis => ctxNode match {
            case XMLRoot(_) => TreeSet() // root does not have a parent
            case node => TreeSet(node.parent)
          }
          // the ancestor axis contains the ancestors of the context node
          // the ancestors of the context node consist of the parent of context node and the parent's parent and so on
          case AncestorAxis => TreeSet[XMLNode]() ++ ctxNode.ancestors
          // the following-sibling axis contains all the following siblings of the context node
          // if the context node is an attribute node or namespace node, the following-sibling axis is empty
          case FollowingSiblingAxis => ???
          // the preceding-sibling axis contains all the preceding siblings of the context node
          // if the context node is an attribute node or namespace node, the preceding-sibling axis is empty
          case PrecedingSiblingAxis => ???
          // the following axis contains all nodes in the same document as the context node that are after the context
          // node in document order, excluding any descendants and excluding attribute nodes and namespace nodes
          case FollowingAxis => ???
          // the preceding axis contains all nodes in the same document as the context node that are before the context
          // node in document order, excluding any ancestors and excluding attribute nodes and namespace nodes
          case PrecedingAxis => ???
          // the attribute axis contains the attributes of the context node; the axis will be empty
          // unless the context node is an element
          case AttributeAxis => ctxNode match {
            case XMLElement(_, attr, _, _) => TreeSet[XMLNode]() ++ attr
          }
          // the namespace axis contains the namespace nodes of the context node
          // the axis will be empty unless the context node is an element
          case NamespaceAxis => throw new NotImplementedError("Namespace nodes are not implemented, therefore the namespace axis is not supported")
          // the self axis contains just the context node itself
          case SelfAxis => TreeSet(ctxNode)
          // the descendant-or-self axis contains the context node and the descendants of the context node
          case DescendantOrSelfAxis => TreeSet(ctxNode) ++ ctxNode.descendants
          // the ancestor-or-self axis contains the context node and the ancestors of the context node
          // thus, the ancestor axis will always include the root node
          case AncestorOrSelfAxis => TreeSet(ctxNode) ++ ctxNode.ancestors
        }
        val testedNodes = nodes.filter {node => first.test match {
          case NameTest("*") => XPathAxis.isPrincipalNodeType(first.axis, node)
          case NameTest(testName) => XPathAxis.isPrincipalNodeType(first.axis, node) && (node match {
            case XMLElement(name, _, _, _) => name == testName
            case XMLAttribute(name, _, _) => name == testName
            case _ => false
          })
          case TextNodeTest => node.isInstanceOf[XMLTextNode]
          case CommentNodeTest => node.isInstanceOf[XMLComment]
          case AllNodeTest => true
        }}
        // TODO: implement predicates (requires distinction between forward and reverse axes; proximity position; see spec section 2.4)
        assert(first.predicates.isEmpty, "Predicates are not yet supported")
        testedNodes.flatMap { n => evaluateLocationPath(n, rest, false, variables)}
      case (Nil, false) => return TreeSet(ctxNode)
    }
  }
}
