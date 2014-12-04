package analysis.domain

import analysis._
import xpath.RelationalOperator

/** An XPath domain, providing operations on XPath values (V). */
trait XPathDomain[V, N, L] {
  /** Get the TOP element */
  def top: V

  /** Get the BOTTOM element */
  def bottom: V

  /** Join two values. This calculates their supremum (least upper bound). */
  def join(v1: V, v2: V): V

  /** Joins a sequence of values. The supremum of the empty list ist BOTTOM. */
  def joinAll(values: Traversable[V]): V = values.fold(bottom)(join)

  /** Compares two elements of the lattice.
    * Returns true if v1 < v2 or v1 = v2, false if v1 > v2 or if they are incomparable.
    */
  def lessThanOrEqual(v1: V, v2: V): Boolean

  /** Compares two elements of the lattice.
    * TOP is always greater than everything else, BOTTOM is always less than everything else.
    */
  def compare(v1: V, v2: V): LatticeOrdering = (lessThanOrEqual(v1, v2), lessThanOrEqual(v2, v1)) match {
    case (true, true) => Equal
    case (true, false) => Less
    case (false, true) => Greater
    case (false, false) => Incomparable
  }

  /** Get the TOP element of the subdomain of numbers (representing any number). topNumber <= top must hold. */
  def topNumber: V

  /** Get the TOP element of the subdomain of strings (representing any string). topString <= top must hold. */
  def topString: V

  /** The addition operation. Must convert its operands to numbers first if they aren't. */
  def add(left: V, right: V): V

  /** The subtraction operation. Must convert its operands to numbers first if they aren't. */
  def subtract(left: V, right: V): V

  /** The multiplication operation. Must convert its operands to numbers first if they aren't. */
  def multiply(left: V, right: V): V

  /** The division operation. Must convert its operands to numbers first if they aren't. */
  def divide(left: V, right: V): V

  /** The modulo operation. Must convert its operands to numbers first if they aren't. */
  def modulo(left: V, right: V): V

  /** Compares two values using a given relational operator (=, !=, <, >, >=, <=).
    * Must behave according to the XPath specification, section 3.4.
    */
  def compareRelational(left: V, right: V, relOp: RelationalOperator): V

  /** The numeric negation operation (unary minus). Must convert its operand to a number if it isn't. */
  def negateNum(v: V): V

  /** Lift a literal string */
  def liftString(lit: String): V

  /** Lift a number */
  def liftNumber(num: Double): V

  /** Lift a boolean */
  def liftBoolean(bool: Boolean): V

  /** The union operator for node-sets. If one of the operands is not a node-set, evaluate to BOTTOM. */
  def nodeSetUnion(left: V, right: V): V

  /** Convert a value to a string as defined by the XPath specification section 4.2. */
  def toStringValue(v: V): V

  /** Convert a value to a boolean as defined by the XPath specification section 4.3. */
  def toBooleanValue(v: V): V

  /** Convert a value to a number as defined by the XPath specification section 4.4. */
  def toNumberValue(v: V): V

  /** Converts a list of nodes to a node-set value.
    * This has to order the nodes in document order and remove duplicates.
    */
  def toNodeSet(list: L): V

  /** Match on a value to find out whether it is a node-set value.
    * The part of the value that is a node-set value is returned as a node list in the first result value,
    * the part of the value that isn't is returned in the second result value.
    */
  def matchNodeSetValues(v: V): (L, V)
 }