package xpath

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
case class NegExpr(inner: XPathExpr) extends XPathExpr
case class FilterExpr(inner: XPathExpr, predicates: List[XPathExpr]) extends XPathExpr
case class FunctionCallExpr(prefix: Option[String], name: String, params: List[XPathExpr]) extends XPathExpr
case class LiteralExpr(literal: String) extends XPathExpr
case class NumberExpr(num: Double) extends XPathExpr
case class VariableReferenceExpr(name: String) extends XPathExpr
case class PathExpr(filter: FilterExpr, locationPath: LocationPath) extends XPathExpr
case class LocationPath(steps: List[XPathStep], isAbsolute: Boolean) extends XPathExpr

/** Base class for relational operators in XPath (=, !=, <, >, <=, >=) */
abstract class RelationalOperator
case object EqualsOperator extends RelationalOperator
case object NotEqualsOperator extends RelationalOperator
case object LessThanOperator extends RelationalOperator
case object GreaterThanOperator extends RelationalOperator
case object LessThanEqualOperator extends RelationalOperator
case object GreaterThanEqualOperator extends RelationalOperator

/** Helper functions for XPath expressions. */
object XPathExpr {

  /** Returns a value indicating whether the given XPath expression is a pattern.
    * Patters are a restricted subset of XPath expressions, see XSLT spec section 5.2.
    * NOTE: id() and key() patterns are not implemented
    */
  def isPattern(expr: XPathExpr): Boolean = {
    expr match {
      case LocationPath(steps, _) => steps.forall {
        // plain '//' operator is allowed and equivalent to descendant-or-self::node()/
        case XPathStep(DescendantOrSelfAxis, AllNodeTest, Nil) => true
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
  def splitUnionPattern(expr: XPathExpr): List[LocationPath] = {
    expr match {
      case UnionExpr(lhs, rhs) => splitUnionPattern(lhs) ++ splitUnionPattern(rhs)
      case pattern@LocationPath(_, _) => List(pattern)
      case _ => throw new IllegalArgumentException(f"$expr is not a pattern")
    }
  }

  /** Returns the default priority of a location path pattern according to the XSLT spec section 5.5
    * and the table at http://www.lenzconsulting.com/how-xslt-works/
    */
  def getDefaultPriority(pattern: LocationPath): Double = {
    if (pattern.steps.size != 1 || pattern.isAbsolute)
      0.5 // more complex patterns or absolute patterns (also matches just '/' which has no steps)
    else pattern.steps.head match {
      case XPathStep(_, NameTest(None, "*"), Nil) => -0.5
      case XPathStep(_, NameTest(Some(_), "*"), Nil) => -0.25
      case XPathStep(_, NameTest(_, _), Nil)
           | XPathStep(_, ProcessingInstructionTest(Some(_)), Nil) => 0
      case XPathStep(ChildAxis | AttributeAxis, AllNodeTest, _)
           | XPathStep(_, CommentNodeTest | TextNodeTest, _)
           | XPathStep(_, ProcessingInstructionTest(None), Nil) => -0.5
      case _ => 0.5
    }
  }
}
