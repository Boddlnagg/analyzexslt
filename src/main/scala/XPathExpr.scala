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

abstract class BinaryExpr(lhs: XPathExpr, rhs: XPathExpr) extends XPathExpr
case class PlusExpr(lhs: XPathExpr, rhs: XPathExpr) extends BinaryExpr(lhs, rhs)
case class MinusExpr(lhs: XPathExpr, rhs: XPathExpr) extends BinaryExpr(lhs, rhs)
case class MultiplyExpr(lhs: XPathExpr, rhs: XPathExpr) extends BinaryExpr(lhs, rhs)
case class DivExpr(lhs: XPathExpr, rhs: XPathExpr) extends BinaryExpr(lhs, rhs)
case class ModExpr(lhs: XPathExpr, rhs: XPathExpr) extends BinaryExpr(lhs, rhs)
case class RelationalExpr(lhs: XPathExpr, rhs: XPathExpr, relOp: RelationalOperator) extends BinaryExpr(lhs, rhs)
case class AndExpr(lhs: XPathExpr, rhs: XPathExpr) extends BinaryExpr(lhs, rhs)
case class OrExpr(lhs: XPathExpr, rhs: XPathExpr) extends BinaryExpr(lhs, rhs)
case class UnionExpr(lhs: XPathExpr, rhs: XPathExpr) extends BinaryExpr(lhs, rhs)
case class NegExpr(expr: XPathExpr) extends XPathExpr
case class FilterExpr(expr: XPathExpr) extends XPathExpr
case class FunctionCallExpr(name: String, params: Seq[XPathExpr]) extends XPathExpr
case class LiteralExpr(literal: String) extends XPathExpr
case class NumberExpr(num: Number) extends XPathExpr
case class VariableReferenceExpr(name: String) extends XPathExpr
case class PathExpr(filter: FilterExpr, locationPath: LocationPath) extends XPathExpr
case class LocationPath(steps: Seq[XPathStep], isAbsolute: Boolean) extends XPathExpr

object XPathExpr {
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
        FunctionCallExpr(callExpr.getFunctionName, callExpr.getParameters.map(p => parse(p.asInstanceOf[Expr])))
      case litExpr: JLiteralExpr => LiteralExpr(litExpr.getLiteral)
      case numExpr: JNumberExpr => NumberExpr(numExpr.getNumber)
      case varRefExpr: JVariableReferenceExpr =>
        assert(varRefExpr.getPrefix == null || varRefExpr.getPrefix.length == 0, "Prefixed variables are not supported")
        VariableReferenceExpr(varRefExpr.getVariableName)
      case pathExpr: JPathExpr =>
        val filter = parse(pathExpr.getFilterExpr)
        assert(filter.isInstanceOf[FilterExpr])
        val locPath = parse(pathExpr.getLocationPath)
        assert(locPath.isInstanceOf[LocationPath])
        PathExpr(filter.asInstanceOf[FilterExpr], locPath.asInstanceOf[LocationPath])
      case locPath: JLocationPath => LocationPath(locPath.getSteps.map(s => XPathStep.parse(s.asInstanceOf[Step])), locPath.isAbsolute)
      case _ => throw new UnsupportedOperationException(f"XPath expression not supported: ${expr.getText}")
    }
  }

  def parse(string: String) : XPathExpr = {
    val reader : XPathReader = XPathReaderFactory.createReader()
    val handler : JaxenHandler = new JaxenHandler()
    reader.setXPathHandler(handler)
    reader.parse(string)
    parse(handler.getXPathExpr.getRootExpr)
  }
}
