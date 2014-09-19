package analysis

import analysis.domain.{XPathDomain, XMLDomain}
import util.EvaluationError
import xml._
import xpath._

import scala.collection.immutable.TreeSet

trait XPathAnalyzer[N, D1 <: XMLDomain[N], T, D2 <: XPathDomain[T]] {
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
      case FunctionCallExpr(name, params) => dom2.evalFunction[N, D1](name, params.map(p => evaluate(p, ctx)), ctx)
      case _ => ??? // TODO
      /*case LocationPath(steps, isAbsolute) => NodeSetValue(evaluateLocationPath(ctx.node, steps, isAbsolute, ctx.variables).toList)
      case PathExpr(filter, locationPath) =>
        evaluate(filter, ctx) match {
          case NodeSetValue(nodes) => NodeSetValue(nodes.flatMap {
            n => evaluateLocationPath(n, locationPath.steps, locationPath.isAbsolute, ctx.variables).toList
          })
          case value => throw new EvaluationError(f"Filter expression must return a node-set (returned: $value)")
        }
      case FilterExpr(subexpr, predicates) =>
        if (!predicates.isEmpty) throw new NotImplementedError("Predicates are not supported")
        evaluate(subexpr, ctx)
      */
    }
  }

  /** Evaluates the steps of a location path.
    *
    * @param ctxNode the context node
    * @param steps the list of remaining steps to evaluate
    * @param isAbsolute a value indicating whether the location path is absolute (or relative)
    * @param variables the set of currently defined variables
    * @return an ordered set of nodes resulting from the location path, ordered in document order
    */
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
        if (!first.predicates.isEmpty) throw new NotImplementedError("Predicates are not supported") // NOTE: see XPath spec section 2.4 to implement these
        testedNodes.flatMap { n => evaluateLocationPath(n, rest, false, variables)}
      case (Nil, false) => TreeSet(ctxNode)
    }
  }
}
