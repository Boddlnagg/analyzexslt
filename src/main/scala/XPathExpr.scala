import java.security.InvalidParameterException

import org.jaxen.JaxenHandler
import org.jaxen.expr.{Expr, Step}
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
                       LocationPath => JLocationPath}
import org.jaxen.saxpath.XPathReader
import org.jaxen.saxpath.helpers.XPathReaderFactory

import scala.collection.JavaConversions._

abstract class RelationalOperator
case object EqualsOperator extends RelationalOperator
case object NotEqualsOperator extends RelationalOperator
case object LessThanOperator extends RelationalOperator
case object GreaterThanOperator extends RelationalOperator
case object LessThanEqualOperator extends RelationalOperator
case object GreaterThanEqualOperator extends RelationalOperator

abstract class XPathExpr

abstract class BinaryExpr extends XPathExpr {
  def lhs: XPathExpr
  def rhs: XPathExpr
}

case class PlusExpr(lhs: XPathExpr, rhs: XPathExpr) extends BinaryExpr
case class MinusExpr(lhs: XPathExpr, rhs: XPathExpr) extends BinaryExpr
case class MultiplyExpr(lhs: XPathExpr, rhs: XPathExpr) extends BinaryExpr
case class DivExpr(lhs: XPathExpr, rhs: XPathExpr) extends BinaryExpr
case class ModExpr(lhs: XPathExpr, rhs: XPathExpr) extends BinaryExpr
case class RelationalExpr(lhs: XPathExpr, rhs: XPathExpr, relOp: RelationalOperator) extends BinaryExpr
case class AndExpr(lhs: XPathExpr, rhs: XPathExpr) extends BinaryExpr
case class OrExpr(lhs: XPathExpr, rhs: XPathExpr) extends BinaryExpr
case class UnionExpr(lhs: XPathExpr, rhs: XPathExpr) extends BinaryExpr
case class NegExpr(expr: XPathExpr) extends XPathExpr
case class FilterExpr(expr: XPathExpr) extends XPathExpr
case class FunctionCallExpr(name: String, params: Seq[XPathExpr]) extends XPathExpr
case class LiteralExpr(literal: String) extends XPathExpr
case class NumberExpr(num: Number) extends XPathExpr
case class VariableReferenceExpr(name: String) extends XPathExpr
case class PathExpr(filter: FilterExpr, locationPath: LocationPath) extends XPathExpr
case class LocationPath(steps: Seq[XPathStep], isAbsolute: Boolean) extends XPathExpr

object XPathExpr {
  def apply(string: String) : XPathExpr = {
    val reader : XPathReader = XPathReaderFactory.createReader()
    val handler : JaxenHandler = new JaxenHandler()
    reader.setXPathHandler(handler)
    reader.parse(string)
    parse(handler.getXPathExpr.getRootExpr)
  }

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
      case filterExpr: JFilterExpr => FilterExpr(parse(filterExpr.getExpr))
      case callExpr: JFunctionCallExpr =>
        assert(callExpr.getPrefix == null || callExpr.getPrefix.length == 0, "Prefixed functions are not supported")
        FunctionCallExpr(callExpr.getFunctionName, callExpr.getParameters.map(p => parse(p.asInstanceOf[Expr])).toList)
      case litExpr: JLiteralExpr => LiteralExpr(litExpr.getLiteral)
      case numExpr: JNumberExpr => NumberExpr(numExpr.getNumber)
      case varRefExpr: JVariableReferenceExpr =>
        assert(varRefExpr.getPrefix == null || varRefExpr.getPrefix.length == 0, "Prefixed variables are not supported")
        VariableReferenceExpr(varRefExpr.getVariableName)
      case pathExpr: JPathExpr =>
        var filter = parse(pathExpr.getFilterExpr)
        if (!filter.isInstanceOf[FilterExpr]) { filter = FilterExpr(filter) }
        val locPath = parse(pathExpr.getLocationPath)
        assert(locPath.isInstanceOf[LocationPath])
        PathExpr(filter.asInstanceOf[FilterExpr], locPath.asInstanceOf[LocationPath])
      case locPath: JLocationPath => LocationPath(locPath.getSteps.map(s => XPathStep.parse(s.asInstanceOf[Step])).toList, locPath.isAbsolute)
      case _ => throw new UnsupportedOperationException(f"XPath expression not supported: ${expr.getText}")
    }
  }

  def isPattern(expr: XPathExpr) : Boolean = {
    // Patterns are a restricted subset of XPath expressions, see spec section 5.2
    // NOTE: id() and key() patterns are not supported
    expr match {
      case LocationPath(steps, _) => steps.forall(s => s match {
        // plain '//' operator is allowed and equivalent to descendant-or-self::node()/
        case AllNodeStep(DescendantOrSelfAxis, List()) => true
        // otherwise only child:: and attribute:: axes are allowed
        case step : XPathStep => step.axis == AttributeAxis || step.axis == ChildAxis
        // all other axes are forbidden
        case _ => false
      })
      case UnionExpr(lhs, rhs) => isPattern(lhs) && isPattern(rhs)
      case _ => false
    }
  }

  def splitUnionPattern(expr: XPathExpr) : List[LocationPath] = {
    expr match {
      case UnionExpr(lhs, rhs) => splitUnionPattern(lhs) ++ splitUnionPattern(rhs)
      case pattern@LocationPath(_, _) => List(pattern)
      case _ => throw new InvalidParameterException(f"$expr is not a pattern")
    }
  }

  def getDefaultPriority(pattern: LocationPath) : Double = {
    // according to spec section 5.5 and the table at http://www.lenzconsulting.com/how-xslt-works/
    // NOTE: prefixed names are not implemented (they would have a default priority of -0.25)
    if (pattern.steps.size != 1 || pattern.isAbsolute)
      0.5 // more complex patterns or absolute patterns (also matches just '/' which has no steps)
    else pattern.steps.head match {
      case NameStep(_, Nil, "*") => -0.5
      case NameStep(_, Nil, _) => 0
      case AllNodeStep(ChildAxis | AttributeAxis, _)
           | CommentNodeStep(_, _)
           | TextNodeStep(_, _)
           | ProcessingInstructionNodeStep(_, _, None) => -0.5
      case ProcessingInstructionNodeStep(_, _, Some(_)) => 0
      case _ => 0.5
    }
  }
}
