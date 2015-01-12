package analysis

import analysis.domain.Domain
import util.ProcessingError
import xpath._

/** Class to analyze XPath expressions using abstract interpretation */
class XPathAnalyzer[N, L, V](dom: Domain[N, L, V]) {
  val xmlDom = dom.xmlDom
  val xpathDom = dom.xpathDom

  /** Evaluates a given XPath expression using a specified abstract context and returns the abstract result. */
  def evaluate(expr: XPathExpr, ctx: AbstractXPathContext[N, L, V]): V = expr match {
    case PlusExpr(lhs, rhs) => xpathDom.add(evaluate(lhs, ctx), evaluate(rhs, ctx))
    case MinusExpr(lhs, rhs) => xpathDom.subtract(evaluate(lhs, ctx), evaluate(rhs, ctx))
    case MultiplyExpr(lhs, rhs) => xpathDom.multiply(evaluate(lhs, ctx), evaluate(rhs, ctx))
    case DivExpr(lhs, rhs) => xpathDom.divide(evaluate(lhs, ctx), evaluate(rhs, ctx))
    case ModExpr(lhs, rhs) => xpathDom.modulo(evaluate(lhs, ctx), evaluate(rhs, ctx))
    case RelationalExpr(lhs, rhs, relOp) => xpathDom.compareRelational(evaluate(lhs, ctx), evaluate(rhs, ctx), relOp)
    case AndExpr(lhs, rhs) =>
      val left = xpathDom.toBooleanValue(evaluate(lhs, ctx))
      val leftMaybeTrue = xpathDom.lessThanOrEqual(xpathDom.liftBoolean(true), left)
      val leftMaybeFalse = xpathDom.lessThanOrEqual(xpathDom.liftBoolean(false), left)
      if (!leftMaybeTrue) {
        // Spec section 3.4: "The right operand is not evaluated if the left operand evaluates to false." (short-circuit evaluation)
        if (!leftMaybeFalse)
          return xpathDom.bottom
        else
          return xpathDom.liftBoolean(false)
      }

      val right = xpathDom.toBooleanValue(evaluate(rhs, ctx))
      val rightMaybeTrue = xpathDom.lessThanOrEqual(xpathDom.liftBoolean(true), right)
      val rightMaybeFalse = xpathDom.lessThanOrEqual(xpathDom.liftBoolean(false), right)
      if (!rightMaybeTrue)
        if (!rightMaybeFalse && !leftMaybeFalse)
          return xpathDom.bottom
        else
          return xpathDom.liftBoolean(false)

      if (!leftMaybeFalse && !rightMaybeFalse)
        return xpathDom.liftBoolean(true)

      xpathDom.join(xpathDom.liftBoolean(true), xpathDom.liftBoolean(false))
    case OrExpr(lhs, rhs) =>
      val left = xpathDom.toBooleanValue(evaluate(lhs, ctx))
      val leftMaybeTrue = xpathDom.lessThanOrEqual(xpathDom.liftBoolean(true), left)
      val leftMaybeFalse = xpathDom.lessThanOrEqual(xpathDom.liftBoolean(false), left)
      if (!leftMaybeFalse) {
        // Spec section 3.4: "The right operand is not evaluated if the left operand evaluates to true." (short-circuit evaluation)
        if (!leftMaybeTrue)
          return xpathDom.bottom
        else
          return xpathDom.liftBoolean(true)
      }

      val right = xpathDom.toBooleanValue(evaluate(rhs, ctx))
      val rightMaybeTrue = xpathDom.lessThanOrEqual(xpathDom.liftBoolean(true), right)
      val rightMaybeFalse = xpathDom.lessThanOrEqual(xpathDom.liftBoolean(false), right)
      if (!rightMaybeFalse)
        if (!rightMaybeTrue && !leftMaybeTrue)
          return xpathDom.bottom
        else
          return xpathDom.liftBoolean(true)

      if (!leftMaybeTrue && !rightMaybeTrue)
        return xpathDom.liftBoolean(false)

      xpathDom.join(xpathDom.liftBoolean(true), xpathDom.liftBoolean(false))
    case UnaryMinusExpr(inner) => xpathDom.negateNum(evaluate(inner, ctx))
    case StringLiteralExpr(literal) => xpathDom.liftString(literal)
    case NumLiteralExpr(num) => xpathDom.liftNumber(num)
    case VarReferenceExpr(name) => try ctx.variables(name) catch {
      // because of static scoping this is an error in the program (no matter what evaluation strategy is used)
      case e: java.util.NoSuchElementException => throw new ProcessingError(f"Variable $name is not defined")
    }
    case UnionExpr(lhs, rhs) => xpathDom.nodeSetUnion(evaluate(lhs, ctx), evaluate(rhs, ctx))
    case FunctionCallExpr(None, name, params) => evaluateFunctionCall(name, params.map(p => evaluate(p, ctx)), ctx)
    case FunctionCallExpr(Some(_), _, _) => throw new NotImplementedError("Prefixed functions are not supported")
    case LocationPath(steps, isAbsolute) => xpathDom.toNodeSet(evaluateLocationPath(ctx.node, steps, isAbsolute))
    case PathExpr(filter, locationPath) =>
      val (startNodeSet, _) = xpathDom.matchNodeSetValues(evaluate(filter, ctx))
      xpathDom.toNodeSet(
        xmlDom.flatMapWithIndex(startNodeSet, {
          case (n, _) => evaluateLocationPath(n, locationPath.steps, locationPath.isAbsolute)
        })
      )
    case FilterExpr(inner, predicates) =>
      if (predicates.nonEmpty) throw new NotImplementedError("Predicates are not supported")
      evaluate(inner, ctx)
  }

  /** Evaluates a function call. See XPath spec sections 3.2 and 4.
    *
    * @param name The name of the function (no prefix).
    * @param params A list of already evaluated function parameters.
    * @param ctx The current XPath evaluation context.
    * @return The value that results from evaluating the function.
    */
  private def evaluateFunctionCall(name: String, params: List[V], ctx: AbstractXPathContext[N, L, V]): V = (name, params) match {
    case ("true", Nil) => xpathDom.liftBoolean(true)
    case ("false", Nil) => xpathDom.liftBoolean(false)
    case ("not", List(arg)) =>
      val bool = xpathDom.toBooleanValue(arg)
      val maybeTrue = xpathDom.lessThanOrEqual(xpathDom.liftBoolean(true), bool)
      val maybeFalse = xpathDom.lessThanOrEqual(xpathDom.liftBoolean(false), bool)
      (maybeTrue, maybeFalse) match {
        case (true, true) => xpathDom.join(xpathDom.liftBoolean(true), xpathDom.liftBoolean(false))
        case (true, false) => xpathDom.liftBoolean(false)
        case (false, true) => xpathDom.liftBoolean(true)
        case (false, false) => xpathDom.bottom
      }
    case ("string", List(arg)) => xpathDom.toStringValue(arg)
    case ("boolean", List(arg)) => xpathDom.toBooleanValue(arg)
    case ("number", List(arg)) => xpathDom.toNumberValue(arg)
    case ("last", Nil) => ctx.size
    case ("position", Nil) => ctx.position
    case ("count", List(arg)) =>
      val (nodeSets, _) = xpathDom.matchNodeSetValues(arg)
      xmlDom.getNodeListSize(nodeSets)

    case ("name"|"local-name", Nil) => xmlDom.getNodeName(ctx.node)
    case ("name"|"local-name", List(arg)) =>
      val (nodeSets, _) = xpathDom.matchNodeSetValues(arg)
      val result = xmlDom.getNodeName(xmlDom.getFirst(nodeSets))
      if (xmlDom.lessThanOrEqualLists(xmlDom.createEmptyList(), nodeSets)) // may the set be empty?
        xpathDom.join(result, xpathDom.liftString("")) // ... then include the empty string in the result
      else
        result
    case ("concat", list@(first :: second :: rest)) => list.reduce(xpathDom.concatStrings) // NOTE: takes 2 or more arguments
    // NOTE: the following functions are more or less stubbed out; implementing them correctly would
    // require adding more methods to the XPath domain interface.
    case ("sum", List(arg)) =>
      val (nodeSets, _) = xpathDom.matchNodeSetValues(arg)
      if (xmlDom.lessThanOrEqualLists(nodeSets, xmlDom.bottomList)) xpathDom.bottom // return bottom if the input is definitely not a node-set
      else xpathDom.topNumber
    case ("string-length", _) => xpathDom.topNumber
    case ("normalize-space", _) => xpathDom.topString
    case _ =>
      throw new ProcessingError(f"Unknown function '$name' (might not be implemented) or invalid number/types of parameters ($params).")
  }

  /** Evaluates the steps of a location path for a single starting node.
    *
    * @param node the context node
    * @param steps the list of remaining steps to evaluate
    * @param isAbsolute a value indicating whether the location path is absolute (or relative)
    * @return an ordered set of nodes resulting from the location path, ordered in document order
    */
  private def evaluateLocationPath(node: N, steps: List[XPathStep], isAbsolute: Boolean): L = {
    // evaluate steps from left to right, keep nodes in document order (not required by XPath, but by XSLT)
    (steps, isAbsolute) match {
      case (_, true) => evaluateLocationPath(xmlDom.getRoot(node), steps, false)
      case (Nil, false) => xmlDom.createSingletonList(node)
      case (first :: rest, false) =>
        val nodes: L = first.axis match {
          // "the child axis contains the children of the context node"
          case ChildAxis => xmlDom.getChildren(node)
          // "the descendant axis contains the descendants of the context node
          // a descendant is a child or a child of a child and so on"
          case DescendantAxis => xmlDom.getDescendants(node)
          // "the parent axis contains the parent of the context node, if there is one"
          case ParentAxis =>
            val (root, nonRoot) = xmlDom.isRoot(node)
            val result = xmlDom.createSingletonList(xmlDom.getParent(nonRoot))
            if (!xmlDom.lessThanOrEqual(root, xmlDom.bottom)) // if the context node may be a root node ...
              xmlDom.joinLists(xmlDom.createEmptyList(), result) // ... we need to include the empty list as a result
            else
              result
          // "the ancestor axis contains the ancestors of the context node
          // the ancestors of the context node consist of the parent of context node and the parent's parent and so on"
          case AncestorAxis => throw new NotImplementedError("The `ancestor` axis is not implemented.")
          // "the following-sibling axis contains all the following siblings of the context node
          // if the context node is an attribute node or namespace node, the following-sibling axis is empty"
          case FollowingSiblingAxis => throw new NotImplementedError("The `following-sibling` axis is not implemented.")
          // "the preceding-sibling axis contains all the preceding siblings of the context node
          // if the context node is an attribute node or namespace node, the preceding-sibling axis is empty"
          case PrecedingSiblingAxis => throw new NotImplementedError("The `preceding-sibling` axis is not implemented.")
          // "the following axis contains all nodes in the same document as the context node that are after the context
          // node in document order, excluding any descendants and excluding attribute nodes and namespace nodes"
          case FollowingAxis => throw new NotImplementedError("The `following` axis is not implemented.")
          // "the preceding axis contains all nodes in the same document as the context node that are before the context
          // node in document order, excluding any ancestors and excluding attribute nodes and namespace nodes"
          case PrecedingAxis => throw new NotImplementedError("The `preceding` axis is not implemented.")
          // "the attribute axis contains the attributes of the context node; the axis will be
          // empty unless the context node is an element"
          case AttributeAxis => xmlDom.getAttributes(node)
          // "the namespace axis contains the namespace nodes of the context node
          // the axis will be empty unless the context node is an element"
          case NamespaceAxis => throw new NotImplementedError("Namespace nodes are not implemented, therefore the namespace axis is not supported")
          // "the self axis contains just the context node itself"
          case SelfAxis => xmlDom.createSingletonList(node)
          // "the descendant-or-self axis contains the context node and the descendants of the context node"
          case DescendantOrSelfAxis => xmlDom.concatLists(xmlDom.createSingletonList(node), xmlDom.getDescendants(node))
          // "the ancestor-or-self axis contains the context node and the ancestors of the context node
          // thus, the ancestor axis will always include the root node"
          case AncestorOrSelfAxis => throw new NotImplementedError("The `ancestor-or-self` axis is not implemented.")
        }
        val testedNodes = xmlDom.filter(nodes, n => {
          first.test match {
            case NameTest(Some(_), _) => throw new NotImplementedError("Prefixed names are not implemented.")
            case NameTest(None, "*") => isPrincipalNodeType(first.axis, n)
            case NameTest(None, testName) =>
              val (correctType, _) = isPrincipalNodeType(first.axis, n)
              xmlDom.hasName(correctType, testName)
            case TextNodeTest => xmlDom.isTextNode(n)
            case CommentNodeTest => xmlDom.isComment(n)
            case AllNodeTest => (n, xmlDom.bottom)
          }
        })
        if (first.predicates.nonEmpty) throw new NotImplementedError("Predicates are not supported") // NOTE: see XPath spec section 2.4 to implement these
        // convert to node-set value and back to L in order to sort the list and remove duplicates
        val (testedNodeSet, _) = xpathDom.matchNodeSetValues(xpathDom.toNodeSet(testedNodes))
        xmlDom.flatMapWithIndex(testedNodeSet, {
          case (n, _) => evaluateLocationPath(n, rest, false)
        })
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
