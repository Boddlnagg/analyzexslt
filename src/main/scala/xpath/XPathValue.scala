package xpath

import xml.XMLNode

import scala.collection.immutable.TreeSet

/** Base class for XPath values.
  *
  * An XPath value can either be a node-set, a boolean, a number or a string (see XPath spec section 1).
  * NOTE: Result tree fragments are an additional data type in the XSLT specification, but they are not implemented here.
  *
  * This class also provides methods to convert between the four value types, where
  * conversions to node-sets are generally not allowed (see XPath spec section 3.3)
  */
abstract class XPathValue {
  /** Converts a value to the boolean type.
    * This matches the boolean() function specified in the XPath spec section 4.3
    */
  def toBooleanValue: BooleanValue = {
    this match {
      case bool: BooleanValue => bool
      case NumberValue(num) => BooleanValue(num == 0 || num.isNaN)
      case NodeSetValue(nodes) => BooleanValue(nodes.nonEmpty)
      case StringValue(str) => BooleanValue(str.length > 0)
    }
  }

  /** Converts a value to the number type.
    * This matches the number() function specified in the XPath spec section 4.4
    */
  def toNumberValue: NumberValue = {
    this match {
      case num: NumberValue => num
      case BooleanValue(bool) => if (bool) NumberValue(1) else NumberValue(0)
      case StringValue(str) => try {
        NumberValue(str.toDouble)
      } catch {
        case e: NumberFormatException => NumberValue(Double.NaN)
      }
      case v: NodeSetValue => v.toStringValue.toNumberValue // first convert to StringValue and then use case defined above
    }
  }

  /** Converts a value to the string type.
    * Except for node-sets with more than one element, this matches the string() function specified in the XPath spec section 4.2
    */
  def toStringValue: StringValue = {
    this match {
      case str: StringValue => str
      case BooleanValue(bool) => StringValue(
        if (bool) "true"
        else "false"
      )
      case NumberValue(num) => StringValue(
        if (num.isNaN) "NaN"
        else if (num.isPosInfinity) "Infinity"
        else if (num.isNegInfinity) "-Infinity"
        else if (num.isValidInt) num.toInt.toString
        else num.toString
      )
      case NodeSetValue(set) => set.toList match {
        case Nil => StringValue("")
        case first :: _ => StringValue(first.stringValue)
      }
    }
  }

  /** Compare two XPath values as specified in the XPath spec section 3.4 */
  def compare(other: XPathValue, relOp: RelationalOperator): Boolean = {
    // comparing node sets is especially tricky and therefore skipped in this implementation
    if (this.isInstanceOf[NodeSetValue] || other.isInstanceOf[NodeSetValue]) throw new NotImplementedError("Comparing node sets is not implemented")
    relOp match {
      case EqualsOperator | NotEqualsOperator =>
        // result depends on types of values
        (this, other) match {
          // at least one operand is a boolean value -> compare both as booleans
          case (_, BooleanValue(_)) | (BooleanValue(_), _) => relOp match {
            case EqualsOperator => this.toBooleanValue.value == other.toBooleanValue.value
            case NotEqualsOperator => this.toBooleanValue.value != other.toBooleanValue.value
          }
          // at least one operand is a number -> compare both as numbers
          case (_, NumberValue(_)) | (NumberValue(_), _) => relOp match {
            case EqualsOperator => this.toNumberValue.value == other.toNumberValue.value
            case NotEqualsOperator => this.toNumberValue.value != other.toNumberValue.value
          }
          // otherwise compare both as strings
          case _ => relOp match {
            case EqualsOperator => this.toStringValue.value == other.toStringValue.value
            case NotEqualsOperator => this.toStringValue.value != other.toStringValue.value
          }
        }
      case LessThanOperator => this.toNumberValue.value < other.toNumberValue.value
      case GreaterThanOperator => this.toNumberValue.value > other.toNumberValue.value
      case LessThanEqualOperator => this.toNumberValue.value <= other.toNumberValue.value
      case GreaterThanEqualOperator => this.toNumberValue.value >= other.toNumberValue.value
    }
  }
}

case class StringValue(value: String) extends XPathValue
case class NumberValue(value: Double) extends XPathValue
case class BooleanValue(value: Boolean) extends XPathValue
case class NodeSetValue(nodes: TreeSet[XMLNode]) extends XPathValue

