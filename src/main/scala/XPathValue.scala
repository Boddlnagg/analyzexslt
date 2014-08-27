/** Base class for XPath values.
  *
  * An XPath value can either be a node-set, a boolean, a number or a string (see XPath spec section 1).
  * NOTE: Result tree fragments are an additional data type in the XSLT specification, but they are not implemented here.
  *
  * This class also provides methods to convert between the four value types, where
  * conversions to node-sets are generally not allowed (see XPath spec section 3.3)*/
abstract class XPathValue {
  /** Converts a value to the boolean type.
    * This matches the boolean() function specified in the XPath spec section 4.3
    */
  def toBooleanValue: BooleanValue = {
    this match {
      case bool@BooleanValue(_) => bool
      case NumberValue(num) => BooleanValue(num == 0 || num.isNaN)
      case NodeSetValue(nodes) => BooleanValue(!nodes.isEmpty)
      case StringValue(str) => BooleanValue(str.length > 0)
    }
  }

  /** Converts a value to the number type.
    * Except for unimplemented cases this matches the number() function specified in the XPath spec section 4.4
    */
  def toNumberValue: NumberValue = {
    //
    this match {
      case num@NumberValue(_) => num
      case BooleanValue(bool) => if (bool) NumberValue(1) else NumberValue(0)
      case _ => throw new NotImplementedError(f"Conversion of $this to number is not implemented.")
    }
  }

  /** Converts a value to the string type.
    * Except node-sets with more than one element, this matches the string() function specified in the XPath spec section 4.2
    */
  def toStringValue: StringValue = {
    this match {
      case str@StringValue(_) => str
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
      case NodeSetValue(Nil) => StringValue("")
      case NodeSetValue(List(node)) => StringValue(node.stringValue)
      case NodeSetValue(_) => throw new NotImplementedError("Converting node sets to strings is not implemented for node-sets with more than one element")
    }
  }
}

case class NodeSetValue(nodes: List[XMLNode]) extends XPathValue
case class BooleanValue(value: Boolean) extends XPathValue
case class NumberValue(value: Double) extends XPathValue
case class StringValue(value: String) extends XPathValue