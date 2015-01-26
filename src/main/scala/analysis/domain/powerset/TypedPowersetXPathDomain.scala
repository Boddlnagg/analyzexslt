package analysis.domain.powerset

import analysis.domain.{XMLDomain, XPathDomain}
import analysis.domain.Lattice
import analysis.domain.Lattice._
import xpath._

case class TypedXPathValue[L](bool: Set[Boolean], // None represents the infinite set, Some represents finite sets
                  num: Option[Set[Double]],
                  str: Option[Set[String]],
                  nodeSet: L)

class TypedPowersetXPathDomain[L] {
  type V = TypedXPathValue[L]

  /** This is the actual (partial) domain implementation */
  trait D[N] extends XPathDomain[V, N, L] {
    val xmlDom: XMLDomain[N, L, V]

    private val latBooleans = BooleanSetLattice
    private val latNumbers = Lattice.createFromOptionalSet[Double]
    private val latStrings = Lattice.createFromOptionalSet[String]

    /** Get the TOP element */
    override def top: V = TypedXPathValue(latBooleans.top, latNumbers.top, latStrings.top, xmlDom.topList)

    /** Get the BOTTOM element */
    override def bottom: V = TypedXPathValue(latBooleans.bottom, latNumbers.bottom, latStrings.bottom, xmlDom.bottomList)

    // "constants" to use for pattern matching
    protected val BOTTOM_NUM: Option[Set[Double]] = latNumbers.bottom
    protected val BOTTOM_STR: Option[Set[String]] = latStrings.bottom
    protected val TOP_BOOL: Set[Boolean] = latBooleans.top

    protected def fromBooleans(bool: Set[Boolean]) = TypedXPathValue(bool, latNumbers.bottom, latStrings.bottom, xmlDom.bottomList)
    protected def fromNumbers(num: Option[Set[Double]]) = TypedXPathValue(latBooleans.bottom, num, latStrings.bottom, xmlDom.bottomList)
    protected def fromStrings(str: Option[Set[String]]) = TypedXPathValue(latBooleans.bottom, latNumbers.bottom, str, xmlDom.bottomList)

    /** Join two values. This calculates their supremum (least upper bound). */
    override def join(v1: V, v2: V): V = TypedXPathValue(
      latBooleans.join(v1.bool, v2.bool),
      latNumbers.join(v1.num, v2.num),
      latStrings.join(v1.str, v2.str),
      xmlDom.joinLists(v1.nodeSet, v2.nodeSet)
    )

    /** Compares two elements of the lattice.
      * Returns true if v1 < v2 or v1 = v2, false if v1 > v2 or if they are incomparable.
      */
    override def lessThanOrEqual(v1: V, v2: V): Boolean = {
      // lessThanOrEqual must hold for all components
      latBooleans.lessThanOrEqual(v1.bool, v2.bool) &&
      latNumbers.lessThanOrEqual(v1.num, v2.num) &&
      latStrings.lessThanOrEqual(v1.str, v2.str) &&
      xmlDom.lessThanOrEqualLists(v1.nodeSet, v2.nodeSet)
    }

    /** Get the TOP element of the subdomain of numbers (representing any number). topNumber <= top must hold. */
    override def topNumber: V = TypedXPathValue(latBooleans.bottom, None, latStrings.bottom, xmlDom.bottomList)

    /** Get the TOP element of the subdomain of strings (representing any string). topString <= top must hold. */
    override def topString: V = TypedXPathValue(latBooleans.bottom, latNumbers.bottom, None, xmlDom.bottomList)

    /** A node-set is converted to a string by returning the string-value of the node in the node-set that is
      * first in document order. If the node-set is empty, an empty string is returned.
      */
    protected def nodeSetToStringValue(nodeSet: L): Option[Set[String]] = {
      val result = xmlDom.getStringValue(xmlDom.getFirst(nodeSet))
      if (xmlDom.lessThanOrEqualLists(xmlDom.createEmptyList(), nodeSet))
        join(result, liftString("")).str
      else
        result.str
    }

    protected def toNumberValueInternal(v: V): Option[Set[Double]] = {
      def stringToNumber(str: String): Double =
        try {
          str.toDouble
        } catch {
          case e: NumberFormatException => Double.NaN
        }

      (v.num, v.str, nodeSetToStringValue(v.nodeSet)) match {
        case (None, _, _) => None
        case (_, None, _) => None
        case (_, _, None) => None
        case (Some(numComponents), Some(str), Some(nodeSet)) =>
          val boolComponents = v.bool.map(b => if (b) 1.0 else 0.0)
          val strComponents = str.map(stringToNumber)
          val nodeSetComponents = nodeSet.map(stringToNumber)
          Some(boolComponents | numComponents | strComponents | nodeSetComponents)
      }
    }

    protected def toBooleanValueInternal(v: V): Set[Boolean] = {
      if (v.bool == TOP_BOOL) TOP_BOOL // boolean part is already TOP
      val TypedXPathValue(_, nodeListSize, _, _) = xmlDom.getNodeListSize(v.nodeSet)

      (v.str, v.num, nodeListSize) match {
        case (None, _, _) => TOP_BOOL
        case (_, None, _) => TOP_BOOL
        case (_, _, None) => TOP_BOOL
        case (Some(str), Some(num), Some(sizes)) =>
          val numComponents = num.map(n => n == 0 || n.isNaN)
          val strComponents = str.map(s => s.length > 0)
          val nodeSetComponents = sizes.map(s => s != 0) // node-set is non-empty
          v.bool | numComponents | strComponents | nodeSetComponents
      }
    }

    protected def toStringValueInternal(v: V): Option[Set[String]] = {
      (v.num, v.str, nodeSetToStringValue(v.nodeSet)) match {
        case (None, _, _) => None
        case (_, None, _) => None
        case (_, _, None) => None
        case (Some(num), Some(strComponents), Some(nodeSetsComponents)) =>
          val boolComponents = v.bool.map(b => if (b) "true" else "false")
          val numComponents = num.map { n =>
            if (n.isNaN) "NaN"
            else if (n.isPosInfinity) "Infinity"
            else if (n.isNegInfinity) "-Infinity"
            else if (n.isValidInt) n.toInt.toString
            else n.toString
          }
          Some(boolComponents | numComponents | strComponents | nodeSetsComponents)
      }
    }

    protected def liftBinaryNumOp(left: V, right: V, f: (Double, Double) => Double): V = {
      val numResult = (toNumberValueInternal(left), toNumberValueInternal(right)) match {
        case (BOTTOM_NUM, _) => BOTTOM_NUM
        case (_, BOTTOM_NUM) => BOTTOM_NUM
        case (Some(s1), Some(s2)) => Some(s1.cross(s2)
          .map { case (v1, v2) => f(v1, v2) }
          .toSet
        )
        case _ => None
      }
      fromNumbers(numResult)
    }

    /** The addition operation. Must convert its operands to numbers first if they aren't. */
    override def add(left: V, right: V): V = liftBinaryNumOp(left, right,
      (v1, v2) => v1 + v2
    )

    /** The subtraction operation. Must convert its operands to numbers first if they aren't. */
    override def subtract(left: V, right: V): V = liftBinaryNumOp(left, right,
      (v1, v2) => v1 - v2
    )

    /** The multiplication operation. Must convert its operands to numbers first if they aren't. */
    override def multiply(left: V, right: V): V = liftBinaryNumOp(left, right,
      (v1, v2) => v1 * v2
    )

    /** The division operation. Must convert its operands to numbers first if they aren't. */
    override def divide(left: V, right: V): V = liftBinaryNumOp(left, right,
      (v1, v2) => v1 / v2
    )

    /** The modulo operation. Must convert its operands to numbers first if they aren't. */
    override def modulo(left: V, right: V): V = liftBinaryNumOp(left, right,
      (v1, v2) => v1 % v2
    )

    /** Compares two values using a given relational operator (=, !=, <, >, >=, <=).
      * Must behave according to the XPath specification, section 3.4.
      */
    override def compareRelational(left: V, right: V, relOp: RelationalOperator): V = {
      // compares for equality
      def compareRelationalBooleans(left: Set[Boolean], right: Set[Boolean]): Set[Boolean] =
      left.cross(right).map {
        case (v1, v2) => v1 == v2
      }.toSet

      // compares for equality
      def compareRelationalStrings(left: Option[Set[String]], right: Option[Set[String]]): Set[Boolean] =
      (left, right) match {
        case (BOTTOM_STR, _) | (_, BOTTOM_STR) => Set[Boolean]() // one operand is BOTTOM -> return BOTTOM
        case (Some(s1), Some(s2)) => s1.cross(s2).map {
          case (v1, v2) => v1 == v2
        }.toSet
        case _ => Set(true, false) // return TOP
      }

      // relOp is any relational operator
      def compareRelationalNumbers(left: Option[Set[Double]], right: Option[Set[Double]], relOp: RelationalOperator): Set[Boolean] =
      (left, right) match {
        case (BOTTOM_NUM, _) | (_, BOTTOM_NUM)  => Set[Boolean]() // one operand is BOTTOM -> return BOTTOM
        case (Some(s1), Some(s2)) => s1.cross(s2).map {
          case (v1, v2) => relOp match {
            case EqualsOperator => v1 == v2
            case NotEqualsOperator => v1 != v2
            case LessThanOperator => v1 < v2
            case GreaterThanOperator => v1 > v2
            case LessThanEqualOperator => v1 <= v2
            case GreaterThanEqualOperator => v1 >= v2
          }
        }.toSet
        case _ => Set(true, false) // return TOP
      }

      if (!xmlDom.lessThanOrEqualLists(left.nodeSet, xmlDom.bottomList) || // if left node-set part is not BOTTOM ...
          !xmlDom.lessThanOrEqualLists(right.nodeSet, xmlDom.bottomList)) { // ... or right node-set part is not BOTTOM ...
        // .. then we don't know the result because node-set comparisons are not implemented
        fromBooleans(Set(true, false))
      } else {
        val result = relOp match {
          case EqualsOperator | NotEqualsOperator =>
            // we can ignore the node-sets now and compare all pairs of remaining components for equality
            val equals =
              // compare both as booleans if at least one of them is a boolean
              compareRelationalBooleans(left.bool, right.bool) |
              compareRelationalBooleans(left.bool, toBooleanValueInternal(fromNumbers(right.num))) |
              compareRelationalBooleans(left.bool, toBooleanValueInternal(fromStrings(right.str))) |
              compareRelationalBooleans(toBooleanValueInternal(fromNumbers(left.num)), right.bool) |
              compareRelationalBooleans(toBooleanValueInternal(fromStrings(left.str)), right.bool) |
              // compare both as numbers if at least one of them is a number (and none is a boolean)
              compareRelationalNumbers(left.num, right.num, EqualsOperator) |
              compareRelationalNumbers(toNumberValueInternal(fromStrings(left.str)), right.num, EqualsOperator) |
              compareRelationalNumbers(left.num, toNumberValueInternal(fromStrings(right.str)), EqualsOperator) |
              // compare both as strings otherwise
              compareRelationalStrings(left.str, right.str)

            if (relOp == NotEqualsOperator) equals.map(b => !b)
            else equals
          case _ => compareRelationalNumbers(toNumberValueInternal(left), toNumberValueInternal(right), relOp)
        }
        fromBooleans(result)
      }
    }

    /** The numeric negation operation (unary minus). Must convert its operand to a number if it isn't. */
    override def negateNum(v: V): V = {
      fromNumbers(toNumberValueInternal(v).map(_.map(num => -num)))
    }

    /** Concatenate two strings. Operands that are not string values are evaluated to BOTTOM. */
    override def concatStrings(left: V, right: V): V = (left.str, right.str) match {
      case (BOTTOM_STR, _) | (_, BOTTOM_STR) => bottom
      case (Some(s1), Some(s2)) => fromStrings(Some(s1.cross(s2).map {
        case (str1, str2) => str1 + str2
      }.toSet))
      case _ => topString // in this case, one parameter is TOP and the other is not BOTTOM
    }

    /** Convert a value to a string as defined by the XPath specification section 4.2. */
    override def toStringValue(v: V): V = fromStrings(toStringValueInternal(v))

    /** Convert a value to a boolean as defined by the XPath specification section 4.3. */
    override def toNumberValue(v: V): V = fromNumbers(toNumberValueInternal(v))

    /** Convert a value to a number as defined by the XPath specification section 4.4. */
    override def toBooleanValue(v: V): V = fromBooleans(toBooleanValueInternal(v))

    /** Lift a literal string */
    override def liftString(lit: String): V = fromStrings(Some(Set(lit)))

    /** Lift a number */
    override def liftNumber(num: Double): V = fromNumbers(Some(Set(num)))

    /** Lift a boolean */
    override def liftBoolean(bool: Boolean): V = fromBooleans(Set(bool))

    /** The union operator for node-sets. If one of the operands is not a node-set, return BOTTOM. */
    override def nodeSetUnion(left: V, right: V): V = {
      val resultSet = nodeListToSet(xmlDom.concatLists(left.nodeSet, right.nodeSet))
      TypedXPathValue(Set(), latNumbers.bottom, latStrings.bottom, resultSet)
    }

    /** Converts a list of nodes to a node-set value.
      * This has to order the nodes in document order and remove duplicates.
      */
    override def toNodeSet(list: L): V = TypedXPathValue(Set(), latNumbers.bottom, latStrings.bottom, nodeListToSet(list))

    /** Match on a value to find out whether it is a node-set value.
      * The part of the value that is a node-set value is returned as a node list in the first result value,
      * the part of the value that isn't is returned in the second result value.
      */
    override def matchNodeSetValues(v: V): (L, V) = (v.nodeSet, TypedXPathValue(v.bool, v.num, v.str, xmlDom.bottomList))

    /** Turn a node list into a set by sorting nodes in document order and removing duplicate nodes */
    def nodeListToSet(list: L): L // must be implemented by subclasses
  }
}
