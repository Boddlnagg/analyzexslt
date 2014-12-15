package xpath

import util.EvaluationError
import xml._

import scala.collection.immutable.TreeSet

/** Object to evaluate XPath expressons */
object XPathEvaluator {
  /** Evaluates a given XPath expression using a specified context and returns the result of the evaluation. */
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
        BooleanValue(lhsVal.compare(rhsVal, relOp))
      // XPath spec section 3.4, shortcut evaluation!
      case AndExpr(lhs, rhs) => BooleanValue(evaluate(lhs, ctx).toBooleanValue.value && evaluate(rhs, ctx).toBooleanValue.value)
      // XPath spec section 3.4, shortcut evaluation!
      case OrExpr(lhs, rhs) => BooleanValue(evaluate(lhs, ctx).toBooleanValue.value || evaluate(rhs, ctx).toBooleanValue.value)

      case NegExpr(subexpr) => NumberValue(- evaluate(subexpr, ctx).toNumberValue.value)
      case LiteralExpr(literal) => StringValue(literal)
      case NumberExpr(num) => NumberValue(num)
      case VariableReferenceExpr(name) => try ctx.variables(name) catch {
        case e: java.util.NoSuchElementException => throw new EvaluationError(f"Variable $name is not defined")
      }
      case UnionExpr(lhs, rhs) => (evaluate(lhs, ctx), evaluate(rhs, ctx)) match {
        case (NodeSetValue(left), NodeSetValue(right)) => NodeSetValue((TreeSet[XMLNode]()++ left ++ right).toList)
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
          case ("sum", List(NodeSetValue(nodes))) => NumberValue(nodes.map(n => StringValue(n.stringValue).toNumberValue.value).sum)
          case ("name"|"local-name", Nil) => ctx.node match {
            // get name of current node (or empty string)
            case XMLElement(nodeName, _, _, _) => StringValue(nodeName)
            case XMLAttribute(nodeName, _, _) => StringValue(nodeName)
            case _ => StringValue("")
          }
          case ("name"|"local-name", List(NodeSetValue(nodes))) => nodes.headOption match {
            case None => StringValue("") // empty list
            case Some(n) => n match {
              case XMLElement(nodeName, _, _, _) => StringValue(nodeName)
              case XMLAttribute(nodeName, _, _) => StringValue(nodeName)
              case _ => StringValue("")
            }
          }
          case (_, evaluatedParams) =>
            throw new EvaluationError(f"Unknown function '$name' (might not be implemented) or invalid number/types of parameters ($evaluatedParams).")
        }
      case LocationPath(steps, isAbsolute) => NodeSetValue(evaluateLocationPathSingle(ctx.node, steps, isAbsolute).toList)
      case PathExpr(filter, locationPath) =>
        evaluate(filter, ctx) match {
          case nodes@NodeSetValue(_) => NodeSetValue(evaluateLocationPath(TreeSet[XMLNode]() ++ nodes.nodes, locationPath.steps, locationPath.isAbsolute).toList)
          case value => throw new EvaluationError(f"Filter expression must return a node-set (returned: $value)")
        }
      case FilterExpr(subexpr, predicates) =>
        if (predicates.nonEmpty) throw new NotImplementedError("Predicates are not supported")
        evaluate(subexpr, ctx)
    }
  }

  /** Evaluates the steps of a location path for a set of starting nodes.
    *
    * @param startNodeSet the set of nodes to start with
    * @param steps the list of remaining steps to evaluate
    * @param isAbsolute a value indicating whether the location path is absolute (or relative)
    * @return an ordered set of nodes resulting from the location path, ordered in document order
    */
  def evaluateLocationPath(startNodeSet: TreeSet[XMLNode], steps: List[XPathStep], isAbsolute: Boolean): TreeSet[XMLNode] =
    startNodeSet.flatMap {
      n => evaluateLocationPathSingle(n, steps, isAbsolute)
    }

  /** Evaluates the steps of a location path for a single starting node.
    *
    * @param ctxNode the context node
    * @param steps the list of remaining steps to evaluate
    * @param isAbsolute a value indicating whether the location path is absolute (or relative)
    * @return an ordered set of nodes resulting from the location path, ordered in document order
    */
  private def evaluateLocationPathSingle(ctxNode: XMLNode, steps: List[XPathStep], isAbsolute: Boolean): TreeSet[XMLNode] = {
    // evaluate steps from left to right, keep nodes in document order (not required by XPath, but by XSLT)
    (steps, isAbsolute) match {
      case (Nil, true) => TreeSet(ctxNode.root)
      case (_, true) => evaluateLocationPathSingle(ctxNode.root, steps, false)
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
          case FollowingSiblingAxis => ctxNode match {
            case XMLAttribute(_, _, _) => TreeSet()
            case _ => ctxNode.parent match {
              case XMLRoot(_) => TreeSet() // if parent is root, there are no siblings
              case XMLElement(_, _, children, _) => TreeSet[XMLNode]() ++ children.filter(_ > ctxNode)
            }
          }
          // the preceding-sibling axis contains all the preceding siblings of the context node
          // if the context node is an attribute node or namespace node, the preceding-sibling axis is empty
          case PrecedingSiblingAxis => ctxNode match {
            case XMLAttribute(_, _, _) => TreeSet()
            case _ => ctxNode.parent match {
              case XMLRoot(_) => TreeSet() // if parent is root, there are no siblings
              case XMLElement(_, _, children, _) => TreeSet[XMLNode]() ++ children.filter(_ < ctxNode)
            }
          }
          // the following axis contains all nodes in the same document as the context node that are after the context
          // node in document order, excluding any descendants and excluding attribute nodes and namespace nodes
          case FollowingAxis =>
            val descendants = ctxNode.descendants
            TreeSet[XMLNode]() ++ ctxNode.root.nodesInOrder.filter(n => !n.isInstanceOf[XMLAttribute] && n > ctxNode && !descendants.contains(n))
          // the preceding axis contains all nodes in the same document as the context node that are before the context
          // node in document order, excluding any ancestors and excluding attribute nodes and namespace nodes
          case PrecedingAxis =>
            val ancestors = ctxNode.ancestors
            TreeSet[XMLNode]() ++ ctxNode.root.nodesInOrder.filter(n => !n.isInstanceOf[XMLAttribute] && n < ctxNode && !ancestors.contains(n))
          // the attribute axis contains the attributes of the context node; the axis will be empty
          // unless the context node is an element
          case AttributeAxis => ctxNode match {
            case XMLElement(_, attr, _, _) => TreeSet[XMLNode]() ++ attr
            case _ => TreeSet[XMLNode]()
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
        if (first.predicates.nonEmpty) throw new NotImplementedError("Predicates are not supported") // NOTE: see XPath spec section 2.4 to implement these
        testedNodes.flatMap { n => evaluateLocationPathSingle(n, rest, false)}
      case (Nil, false) => TreeSet(ctxNode)
    }
  }
}
