package xpath

import org.jaxen.saxpath.Axis
import org.jaxen.JaxenHandler
import org.jaxen.expr.{Expr, Step, Predicate}
import org.jaxen.expr.{AdditiveExpr => JAdditiveExpr,
                       MultiplicativeExpr => JMultiplicativeExpr,
                       EqualityExpr => JEqualityExpr,
                       RelationalExpr => JRelationalExpr,
                       LogicalExpr => JLogicalExpr,
                       UnionExpr => JUnionExpr,
                       UnaryExpr => JUnaryExpr,
                       FilterExpr => JFilterExpr,
                       FunctionCallExpr => JFunctionCallExpr,
                       LiteralExpr => JLiteralExpr,
                       NumberExpr => JNumberExpr,
                       VariableReferenceExpr => JVariableReferenceExpr,
                       PathExpr => JPathExpr,
                       LocationPath => JLocationPath,
                       AllNodeStep => JAllNodeStep,
                       CommentNodeStep => JCommentNodeStep,
                       ProcessingInstructionNodeStep => JProcessingInstructionNodeStep,
                       TextNodeStep => JTextNodeStep,
                       NameStep => JNameStep}
import org.jaxen.saxpath.XPathReader
import org.jaxen.saxpath.helpers.XPathReaderFactory

import scala.collection.JavaConversions._

object XPathParser {
  /** Creates an XPath expression by parsing a string */
  def parse(string: String): XPathExpr = {
    val reader: XPathReader = XPathReaderFactory.createReader()
    val handler: JaxenHandler = new JaxenHandler()
    reader.setXPathHandler(handler)
    reader.parse(string)
    parse(handler.getXPathExpr.getRootExpr)
  }

  /** Parses a Jaxen XPath expression and returns an equivalent [[XPathExpr]]*/
  def parse(expr: Expr): XPathExpr = {
    expr match {
      case addExpr: JAdditiveExpr =>
        addExpr.getOperator match {
          case "+" => PlusExpr(parse(addExpr.getLHS), parse(addExpr.getRHS))
          case "-" => MinusExpr(parse(addExpr.getLHS), parse(addExpr.getRHS))
        }
      case mulExpr: JMultiplicativeExpr =>
        mulExpr.getOperator match {
          case "*" => MultiplyExpr(parse(mulExpr.getLHS), parse(mulExpr.getRHS))
          case "div" => DivExpr(parse(mulExpr.getLHS), parse(mulExpr.getRHS))
          case "mod" => ModExpr(parse(mulExpr.getLHS), parse(mulExpr.getRHS))
        }
      case eqExpr: JEqualityExpr =>
        eqExpr.getOperator match {
          case "=" => RelationalExpr(parse(eqExpr.getLHS), parse(eqExpr.getRHS), EqualsOperator)
          case "!=" => RelationalExpr(parse(eqExpr.getLHS), parse(eqExpr.getRHS), NotEqualsOperator)
        }
      case relExpr: JRelationalExpr =>
        relExpr.getOperator match {
          case "<" => RelationalExpr(parse(relExpr.getLHS), parse(relExpr.getRHS), LessThanOperator)
          case ">" => RelationalExpr(parse(relExpr.getLHS), parse(relExpr.getRHS), GreaterThanOperator)
          case "<=" => RelationalExpr(parse(relExpr.getLHS), parse(relExpr.getRHS), LessThanEqualOperator)
          case ">=" => RelationalExpr(parse(relExpr.getLHS), parse(relExpr.getRHS), GreaterThanEqualOperator)
        }
      case logExpr: JLogicalExpr =>
        logExpr.getOperator match {
          case "and" => AndExpr(parse(logExpr.getLHS), parse(logExpr.getRHS))
          case "or" => OrExpr(parse(logExpr.getLHS), parse(logExpr.getRHS))
        }
      case unionExpr: JUnionExpr => UnionExpr(parse(unionExpr.getLHS), parse(unionExpr.getRHS))
      case unaryExpr: JUnaryExpr => NegExpr(parse(unaryExpr.getExpr))
      case filterExpr: JFilterExpr => FilterExpr(
        parse(filterExpr.getExpr),
        filterExpr.getPredicates.map(p => parse(p.asInstanceOf[Predicate].getExpr)).toList
      )
      case callExpr: JFunctionCallExpr =>
        val prefix = if (callExpr.getPrefix != null && callExpr.getPrefix.length > 0) Some(callExpr.getPrefix) else None
        FunctionCallExpr(prefix, callExpr.getFunctionName, callExpr.getParameters.map(p => parse(p.asInstanceOf[Expr])).toList)
      case litExpr: JLiteralExpr => LiteralExpr(litExpr.getLiteral)
      case numExpr: JNumberExpr => NumberExpr(numExpr.getNumber.doubleValue())
      case varRefExpr: JVariableReferenceExpr =>
        if (varRefExpr.getPrefix != null && varRefExpr.getPrefix.length > 0) throw new NotImplementedError("Prefixed variables are not supported")
        VariableReferenceExpr(varRefExpr.getVariableName)
      case pathExpr: JPathExpr =>
        var filter = parse(pathExpr.getFilterExpr)
        if (!filter.isInstanceOf[FilterExpr]) { filter = FilterExpr(filter, Nil) }
        val locPath = parse(pathExpr.getLocationPath)
        assert(locPath.isInstanceOf[LocationPath])
        PathExpr(filter.asInstanceOf[FilterExpr], locPath.asInstanceOf[LocationPath])
      case locPath: JLocationPath => LocationPath(locPath.getSteps.map(s => parseStep(s.asInstanceOf[Step])).toList, locPath.isAbsolute)
      case _ => throw new NotImplementedError(f"XPath expression not supported: ${expr.getText}")
    }
  }

  /** Parses a Jaxen [[Step]] and returns an equivalent [[XPathStep]] */
  def parseStep(step: Step): XPathStep = {
    val axis = parseAxis(step.getAxis)
    val predicates = step.getPredicates.map(p => parse(p.asInstanceOf[Predicate].getExpr)).toList
    val nodeTest = step match {
      // ::node()
      case allNode: JAllNodeStep => AllNodeTest
      // ::comment()
      case commentNode: JCommentNodeStep => CommentNodeTest
      // ::text()
      case textNode: JTextNodeStep => TextNodeTest
      // any name (might also be '*')
      case nameStep: JNameStep =>
        val prefix = if (nameStep.getPrefix != null && nameStep.getPrefix.length > 0) Some(nameStep.getPrefix) else None
        NameTest(prefix, nameStep.getLocalName)
      // ::processing-instruction() OR ::processing-instruction('name')
      case piNode: JProcessingInstructionNodeStep =>
        val name = if (piNode.getName != null && piNode.getName.length > 0) Some(piNode.getName) else None
        ProcessingInstructionTest(name)
    }
    XPathStep(axis, nodeTest, predicates)
  }

  /** Creates an axis object from a Jaxen axis identifier */
  def parseAxis(axis: Int): XPathAxis = axis match {
    case Axis.CHILD => ChildAxis
    case Axis.DESCENDANT => DescendantAxis
    case Axis.PARENT => ParentAxis
    case Axis.FOLLOWING_SIBLING => FollowingSiblingAxis
    case Axis.PRECEDING_SIBLING => PrecedingSiblingAxis
    case Axis.FOLLOWING => FollowingAxis
    case Axis.PRECEDING => PrecedingAxis
    case Axis.ATTRIBUTE => AttributeAxis
    case Axis.NAMESPACE => NamespaceAxis
    case Axis.SELF => SelfAxis
    case Axis.DESCENDANT_OR_SELF => DescendantOrSelfAxis
    case Axis.ANCESTOR_OR_SELF => AncestorOrSelfAxis
    case Axis.ANCESTOR => AncestorAxis
  }
}
