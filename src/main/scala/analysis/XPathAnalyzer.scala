package analysis

import analysis.domain.Domain
import util.EvaluationError
import xml._
import xpath._

import scala.collection.immutable.TreeSet

class XPathAnalyzer[N, L, V](dom: Domain[N, L, V]) {
  val xmlDom = dom.xmlDom
  val xpathDom = dom.xpathDom

  /** Evaluates a given XPath expression using a specified context and returns the result of the evaluation. */
  def evaluate(expr: XPathExpr, ctx: AbstractXPathContext[N, L, V]): V = {
    expr match {
      case PlusExpr(lhs, rhs) => xpathDom.add(evaluate(lhs, ctx), evaluate(rhs, ctx))
      case MinusExpr(lhs, rhs) => xpathDom.subtract(evaluate(lhs, ctx), evaluate(rhs, ctx))
      case MultiplyExpr(lhs, rhs) => xpathDom.multiply(evaluate(lhs, ctx), evaluate(rhs, ctx))
      case DivExpr(lhs, rhs) => xpathDom.divide(evaluate(lhs, ctx), evaluate(rhs, ctx))
      case ModExpr(lhs, rhs) => xpathDom.modulo(evaluate(lhs, ctx), evaluate(rhs, ctx))
      case RelationalExpr(lhs, rhs, relOp) => xpathDom.compare(evaluate(lhs, ctx), evaluate(rhs, ctx), relOp)
      case AndExpr(lhs, rhs) => xpathDom.logicalAnd(evaluate(lhs, ctx), evaluate(rhs, ctx))
      case OrExpr(lhs, rhs) => xpathDom.logicalOr(evaluate(lhs, ctx), evaluate(rhs, ctx))
      case NegExpr(subexpr) => xpathDom.negateNum(evaluate(subexpr, ctx))
      case LiteralExpr(literal) => xpathDom.liftLiteral(literal)
      case NumberExpr(num) => xpathDom.liftNumber(num)
      case VariableReferenceExpr(name) => try ctx.variables(name) catch {
        // because of static scoping this is an error in the program (no matter what evaluation strategy is used)
        case e: java.util.NoSuchElementException => throw new EvaluationError(f"Variable $name is not defined")
      }
      case UnionExpr(lhs, rhs) => xpathDom.nodeSetUnion(evaluate(lhs, ctx), evaluate(rhs, ctx))
      case FunctionCallExpr(name, params) => (name, params.map(p => evaluate(p, ctx))) match {
        case ("true", Nil) => xpathDom.liftBoolean(true)
        case ("false", Nil) => xpathDom.liftBoolean(false)
        case ("not", List(arg)) => xpathDom.negateBool(arg)
        case ("string", List(arg)) => xpathDom.toStringValue(arg)
        case ("boolean", List(arg)) => xpathDom.toBooleanValue(arg)
        case ("number", List(arg)) => xpathDom.toNumberValue(arg)
        case ("last", Nil) => ctx.size
        case ("position", Nil) => ctx.position
        case ("count", List(arg)) =>
          val (nodeSets, _) = xpathDom.matchNodeSetValues(arg)
          xmlDom.getNodeListSize(nodeSets)
        case ("sum", List(arg)) =>
          val (nodeSets, _) = xpathDom.matchNodeSetValues(arg)
          if (nodeSets == xmlDom.bottomList) xpathDom.bottom // return bottom if the input is definitely not a node-set
          else xpathDom.top // TODO: implement this correctly? (could also return topNumber if available)
        case ("name"|"local-name", Nil) => xmlDom.getNodeName(ctx.node)
        case (_, evaluatedParams) =>
          throw new EvaluationError(f"Unknown function '$name' (might not be implemented) or invalid number/types of parameters ($evaluatedParams).")
      }
      case LocationPath(steps, isAbsolute) => xpathDom.toNodeSet(evaluateLocationPathSingle(ctx.node, steps, isAbsolute))
      case PathExpr(filter, locationPath) =>
        val (startNodeSet, _) = xpathDom.matchNodeSetValues(evaluate(filter, ctx))
        xpathDom.toNodeSet(evaluateLocationPath(startNodeSet, locationPath.steps, locationPath.isAbsolute))
      case FilterExpr(subexpr, predicates) =>
        if (!predicates.isEmpty) throw new NotImplementedError("Predicates are not supported")
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
  def evaluateLocationPath(startNodeSet: L, steps: List[XPathStep], isAbsolute: Boolean): L =
    xmlDom.flatMapWithIndex(startNodeSet, {
      case (n, _) => evaluateLocationPathSingle(n, steps, isAbsolute)
    })

  /** Evaluates the steps of a location path for a single starting node.
    *
    * @param ctxNode the context node
    * @param steps the list of remaining steps to evaluate
    * @param isAbsolute a value indicating whether the location path is absolute (or relative)
    * @return an ordered set of nodes resulting from the location path, ordered in document order
    */
  def evaluateLocationPathSingle(ctxNode: N, steps: List[XPathStep], isAbsolute: Boolean): L = {
    // TODO: implement more axes WITH TESTS!
    // evaluate steps from left to right, keep nodes in document order (not required by XPath, but by XSLT)
    (steps, isAbsolute) match {
      case (Nil, true) => xmlDom.createSingletonList(xmlDom.getRoot(ctxNode))
      case (_, true) => evaluateLocationPathSingle(xmlDom.getRoot(ctxNode), steps, false)
      case (first :: rest, false) =>
        val nodes: L = first.axis match {
          // the child axis contains the children of the context node
          case ChildAxis => xmlDom.getChildren(ctxNode)
          // the descendant axis contains the descendants of the context node
          // a descendant is a child or a child of a child and so on
          case DescendantAxis => xmlDom.getDescendants(ctxNode)
          /*
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
          */
          // the attribute axis contains the attributes of the context node; the axis will be empty
          // unless the context node is an element
          case AttributeAxis => xmlDom.getAttributes(ctxNode)
          // the namespace axis contains the namespace nodes of the context node
          // the axis will be empty unless the context node is an element
          case NamespaceAxis => throw new NotImplementedError("Namespace nodes are not implemented, therefore the namespace axis is not supported")
          // the self axis contains just the context node itself
          case SelfAxis => xmlDom.createSingletonList(ctxNode)
          // the descendant-or-self axis contains the context node and the descendants of the context node
          case DescendantOrSelfAxis => xmlDom.concatLists(xmlDom.createSingletonList(ctxNode), xmlDom.getDescendants(ctxNode))
          /*
          // the ancestor-or-self axis contains the context node and the ancestors of the context node
          // thus, the ancestor axis will always include the root node
          case AncestorOrSelfAxis => TreeSet(ctxNode) ++ ctxNode.ancestors
          */
        }
        val testedNodes = xmlDom.filter(nodes, node => {
          first.test match {
            case NameTest("*") => isPrincipalNodeType(first.axis, node)
            case NameTest(testName) =>
              val (correctType, _) = isPrincipalNodeType(first.axis, node)
              xmlDom.hasName(correctType, testName)
            case TextNodeTest => xmlDom.isTextNode(node)
            case CommentNodeTest => xmlDom.isComment(node)
            case AllNodeTest => (node, xmlDom.bottom)
          }
        })
        if (!first.predicates.isEmpty) throw new NotImplementedError("Predicates are not supported") // NOTE: see XPath spec section 2.4 to implement these
        // convert to node-set value and back to L in order to sort the list and remove duplicates
        val (testedNodeSet, _) = xpathDom.matchNodeSetValues(xpathDom.toNodeSet(testedNodes))
        xmlDom.flatMapWithIndex(testedNodeSet, {
          case (node, _) => evaluateLocationPathSingle(node, rest, false)
        })
      case (Nil, false) => xmlDom.createSingletonList(ctxNode)
    }
  }

  /** Returns a value indicating whether a node is of the principal node type of a given axis (see XPath spec section 2.3) */
  def isPrincipalNodeType(axis: XPathAxis, node: N): (N, N) = {
    axis match {
      case AttributeAxis => xmlDom.isAttribute(node)
      case NamespaceAxis => (xmlDom.bottom, node) // namespace nodes are not supported
      case _ => xmlDom.isElement(node)
    }
  }
}
