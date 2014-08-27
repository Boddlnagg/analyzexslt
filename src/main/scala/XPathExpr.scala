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
                       LocationPath => JLocationPath}
import org.jaxen.saxpath.XPathReader
import org.jaxen.saxpath.helpers.XPathReaderFactory

import scala.collection.JavaConversions._

/** Base class for relational operators in XPath (=, !=, <, >, <=, >=) */
abstract class RelationalOperator
case object EqualsOperator extends RelationalOperator
case object NotEqualsOperator extends RelationalOperator
case object LessThanOperator extends RelationalOperator
case object GreaterThanOperator extends RelationalOperator
case object LessThanEqualOperator extends RelationalOperator
case object GreaterThanEqualOperator extends RelationalOperator

/** Base class for XPath expressions */
abstract class XPathExpr

/** Base class for binary XPath expressions */
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
case class FilterExpr(expr: XPathExpr, predicates: List[XPathExpr]) extends XPathExpr
case class FunctionCallExpr(name: String, params: List[XPathExpr]) extends XPathExpr
case class LiteralExpr(literal: String) extends XPathExpr
case class NumberExpr(num: Double) extends XPathExpr
case class VariableReferenceExpr(name: String) extends XPathExpr
case class PathExpr(filter: FilterExpr, locationPath: LocationPath) extends XPathExpr
case class LocationPath(steps: List[XPathStep], isAbsolute: Boolean) extends XPathExpr

/** Factory for [[XPathExpr]] instances */
object XPathExpr {
  /** Creates an XPath expression by parsing a string */
  def apply(string: String) : XPathExpr = {
    val reader : XPathReader = XPathReaderFactory.createReader()
    val handler : JaxenHandler = new JaxenHandler()
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
        if (callExpr.getPrefix != null && callExpr.getPrefix.length > 0) throw new NotImplementedError("Prefixed functions are not supported")
        FunctionCallExpr(callExpr.getFunctionName, callExpr.getParameters.map(p => parse(p.asInstanceOf[Expr])).toList)
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
      case locPath: JLocationPath => LocationPath(locPath.getSteps.map(s => XPathStep.parse(s.asInstanceOf[Step])).toList, locPath.isAbsolute)
      case _ => throw new NotImplementedError(f"XPath expression not supported: ${expr.getText}")
    }
  }

  /** Returns a value indicating whether the given XPath expression is a pattern.
    * Patters are a restricted subset of XPath expressions, see XSLT spec section 5.2.
    * NOTE: id() and key() patterns are not implemented
    */
  def isPattern(expr: XPathExpr) : Boolean = {
    expr match {
      case LocationPath(steps, _) => steps.forall {
        // plain '//' operator is allowed and equivalent to descendant-or-self::node()/
        case XPathStep(DescendantOrSelfAxis, AllNodeTest, List()) => true
        // otherwise only child:: and attribute:: axes are allowed
        case XPathStep(AttributeAxis | ChildAxis, _, _) => true
        // all other axes are forbidden
        case _ => false
      }
      case UnionExpr(lhs, rhs) => isPattern(lhs) && isPattern(rhs)
      case _ => false
    }
  }

  /** Splits an XPath expression that is a pattern into its parts.
    * The resulting list contains every sub-expression originally separated by the union operator.
    */
  def splitUnionPattern(expr: XPathExpr) : List[LocationPath] = {
    expr match {
      case UnionExpr(lhs, rhs) => splitUnionPattern(lhs) ++ splitUnionPattern(rhs)
      case pattern@LocationPath(_, _) => List(pattern)
      case _ => throw new IllegalArgumentException(f"$expr is not a pattern")
    }
  }

  /** Returns the default priority of a location path pattern according to the XSLT spec section 5.5
    * and the table at http://www.lenzconsulting.com/how-xslt-works/
    *
    * NOTE: prefixed names are not implemented (they would have a default priority of -0.25),
    *       processing instruction node tests are also not implemented (they would have a default priority
    *       of -0.5 or 0 depending on whether they match a specific name)
    */
  def getDefaultPriority(pattern: LocationPath) : Double = {
    if (pattern.steps.size != 1 || pattern.isAbsolute)
      0.5 // more complex patterns or absolute patterns (also matches just '/' which has no steps)
    else pattern.steps.head match {
      case XPathStep(_, NameTest("*"), Nil) => -0.5
      case XPathStep(_, NameTest(_), Nil) => 0
      case XPathStep(ChildAxis | AttributeAxis, AllNodeTest, _)
           | XPathStep(_, CommentNodeTest | TextNodeTest, _) => -0.5
      case _ => 0.5
    }
  }
}
