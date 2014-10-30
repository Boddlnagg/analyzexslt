package analysis.domain.concrete

import analysis.domain.XPathDomain
import xml.XMLNode
import xpath._

import scala.collection.immutable.TreeSet

/** Just a wrapper for the type alias */
object ConcreteXPathDomain {
  type V = SingleValueLattice[XPathValue]

  /** This is the actual (partial) domain implementation */
  trait D[N, L] extends XPathDomain[V, N, L] {
    /** Get the TOP element */
    override def top: V = Top

    /** Get the BOTTOM element */
    override def bottom: V = Bottom

    /** Join two values. This calculates their supremum (least upper bound). */
    override def join(v1: V, v2: V): V = v1.join(v2)

    /** Compares two elements of the lattice.
      * Returns true if v1 < v2 or v1 = v2, false if v1 > v2 or if they are incomparable.
      */
    override def lessThanOrEqual(v1: V, v2: V): Boolean = v1.lessThanOrEqual(v2)

    /** Get the TOP element of the subdomain of numbers (representing any number). topNumber <= top must hold. */
    override def topNumber: V = Top // no type distinction in this domain

    /** Get the TOP element of the subdomain of strings (representing any string). topString <= top must hold. */
    override def topString: V = Top // no type distinction in this domain

    protected def liftBinaryNumOp(left: V, right: V)(f: (Double, Double) => Double): V =
        left.liftBinaryOp(right) {(v1, v2) => NumberValue(f(v1.toNumberValue.value, v2.toNumberValue.value))}

    /** The addition operation. Must convert its operands to numbers first if they aren't. */
    override def add(left: V, right: V): V = liftBinaryNumOp(left, right) {(v1, v2) => v1 + v2}

    /** The subtraction operation. Must convert its operands to numbers first if they aren't. */
    override def subtract(left: V, right: V): V = liftBinaryNumOp(left, right) {(v1, v2) => v1 - v2}

    /** The multiplication operation. Must convert its operands to numbers first if they aren't. */
    override def multiply(left: V, right: V): V = liftBinaryNumOp(left, right) {(v1, v2) => v1 * v2}

    /** The division operation. Must convert its operands to numbers first if they aren't. */
    override def divide(left: V, right: V): V = liftBinaryNumOp(left, right) {(v1, v2) => v1 / v2}

    /** The modulo operation. Must convert its operands to numbers first if they aren't. */
    override def modulo(left: V, right: V): V = liftBinaryNumOp(left, right) {(v1, v2) => v1 % v2}

    /** Compares two values using a given relational operator (=, !=, <, >, >=, <=).
      * Must behave according to the XPath specification, section 3.4.
      */
    override def compareRelational(left: V, right: V, relOp: RelationalOperator): V = left.liftBinaryOp(right) {
      (v1, v2) => BooleanValue(v1.compare(v2, relOp))
    }

    /** The numeric negation operation (unary minus). Must convert its operand to a number if it isn't. */
    override def negateNum(v: V): V = v.map(n => NumberValue(-n.toNumberValue.value))

    /** Lift a literal string */
    override def liftLiteral(lit: String): V = Value(StringValue(lit))

    /** Lift a number */
    override def liftNumber(num: Double): V = Value(NumberValue(num))

    /** Lift a boolean */
    override def liftBoolean(bool: Boolean): V = Value(BooleanValue(bool))

    /** The union operator for node-sets. If one of the operands is not a node-set, return BOTTOM. */
    override def nodeSetUnion(left: V, right: V): V = (left, right) match {
      case (Value(NodeSetValue(lVal)), Value(NodeSetValue(rVal))) =>
        Value(NodeSetValue((TreeSet[XMLNode]() ++ lVal ++ rVal).toList))
      case (Value(_), Value(_)) => Bottom // values of incompatible type -> error/bottom
      case (Bottom, _) => Bottom
      case (_, Bottom) => Bottom
      case _ => Top
    }

    /** Convert a value to a string as defined by the XPath specification section 4.2. */
    override def toStringValue(v: V): V = v.map(_.toStringValue)

    /** Convert a value to a boolean as defined by the XPath specification section 4.3. */
    override def toBooleanValue(v: V): V = v.map(_.toBooleanValue)

    /** Convert a value to a number as defined by the XPath specification section 4.4. */
    override def toNumberValue(v: V): V = v.map(_.toNumberValue)
  }
}
