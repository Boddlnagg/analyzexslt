package analysis.domain.powerset

import analysis.domain.{XMLDomain, XPathDomain}
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

    override def top: V = TypedXPathValue(Set(true, false), None, None, xmlDom.topList)
    override def bottom: V = TypedXPathValue(Set(), bottomComponent, bottomComponent, xmlDom.bottomList)

    protected val BOTTOM_NUM: Option[Set[Double]] = Some(Set())

    override def join(v1: V, v2: V): V = TypedXPathValue(
      v1.bool.union(v2.bool),
      joinComponent(v1.num, v2.num),
      joinComponent(v1.str, v2.str),
      xmlDom.joinList(v1.nodeSet, v2.nodeSet) // TODO: need to keep list sorted explicitly?
    )

    protected def bottomComponent[T]: Option[Set[T]] = Some(Set())

    protected def joinComponent[T](lhs: Option[Set[T]], rhs: Option[Set[T]]): Option[Set[T]] = (lhs, rhs) match {
      case (None, _) => None
      case (_, None) => None
      case (Some(s1), Some(s2)) => Some(s1.union(s2))
    }

    protected def lessThanOrEqualComponent[T](lhs: Option[Set[T]], rhs: Option[Set[T]]): Boolean = (lhs, rhs) match {
      case (_, None) => true
      case (None, Some(_)) => false
      case (Some(s1), Some(s2)) => s1.subsetOf(s2)
    }

    /*override def meet(v1: V, v2: V): V = (v1, v2) match {
      case (None, _) => None
      case (_, None) => None
      case (Some(s1), Some(s2)) => Some(s1.intersect(s2))
    }*/

    override def lessThanOrEqual(v1: V, v2: V): Boolean = {
      // lessThanOrEqual must hold for all components
      v1.bool.subsetOf(v2.bool) &&
      lessThanOrEqualComponent(v1.num, v2.num) &&
      lessThanOrEqualComponent(v1.str, v2.str) &&
      xmlDom.lessThanOrEqualList(v1.nodeSet, v2.nodeSet)
    }

    override def topNumber: V = TypedXPathValue(Set(), None, bottomComponent, xmlDom.topList)
    override def topString: V = TypedXPathValue(Set(), bottomComponent, None, xmlDom.topList)

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
          Some(boolComponents union numComponents union strComponents union nodeSetComponents)
      }
    }

    protected def toBooleanValueInternal(v: V): Set[Boolean] = {
      if (v.bool == Set(true, false)) Set(true, false) // boolean part is already TOP
      val TypedXPathValue(_, nodeListSize, _, _) = xmlDom.getNodeListSize(v.nodeSet)

      (v.str, v.num, nodeListSize) match {
        case (None, _, _) => Set(true, false)
        case (_, None, _) => Set(true, false)
        case (_, _, None) => Set(true, false)
        case (Some(str), Some(num), Some(sizes)) =>
          val numComponents = num.map(n => n == 0 || n.isNaN)
          val strComponents = str.map(s => s.length > 0)
          val nodeSetComponents = sizes.map(s => s != 0) // node-set is non-empty
          v.bool union numComponents union strComponents union nodeSetComponents
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
          Some(boolComponents union numComponents union strComponents union nodeSetsComponents)
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
      TypedXPathValue(Set(), numResult, bottomComponent[String], xmlDom.bottomList)
    }

    override def add(left: V, right: V): V = liftBinaryNumOp(left, right,
      (v1, v2) => v1 + v2
    )

    override def subtract(left: V, right: V): V = liftBinaryNumOp(left, right,
      (v1, v2) => v1 - v2
    )

    override def multiply(left: V, right: V): V = liftBinaryNumOp(left, right,
      (v1, v2) => v1 * v2
    )

    override def divide(left: V, right: V): V = liftBinaryNumOp(left, right,
      (v1, v2) => v1 / v2
    )

    override def modulo(left: V, right: V): V = liftBinaryNumOp(left, right,
      (v1, v2) => v1 % v2
    )

    override def compareRelational(left: V, right: V, relOp: RelationalOperator): V = ??? // TODO

    override def negateNum(v: V): V = {
      val numResult = toNumberValueInternal(v).map(_.map(num => -num))
      TypedXPathValue(Set(), numResult, bottomComponent[String], xmlDom.bottomList)
    }

    override def toStringValue(v: V): V = TypedXPathValue(Set[Boolean](), bottomComponent[Double], toStringValueInternal(v), xmlDom.bottomList)
    override def toNumberValue(v: V): V = TypedXPathValue(Set[Boolean](), toNumberValueInternal(v), bottomComponent[String], xmlDom.bottomList)
    override def toBooleanValue(v: V): V = TypedXPathValue(toBooleanValueInternal(v), bottomComponent[Double], bottomComponent[String], xmlDom.bottomList)

    override def liftLiteral(lit: String): V =
      TypedXPathValue(Set(), bottomComponent[Double], Some(Set(lit)), xmlDom.bottomList)

    override def liftNumber(num: Double): V =
      TypedXPathValue(Set(), Some(Set(num)), bottomComponent[String], xmlDom.bottomList)

    override def liftBoolean(bool: Boolean): V =
      TypedXPathValue(Set(bool), bottomComponent[Double], bottomComponent[String], xmlDom.bottomList)

    override def nodeSetUnion(left: V, right: V): V = {
      val resultSet = nodeListToSet(xmlDom.concatLists(left.nodeSet, right.nodeSet))
      TypedXPathValue(Set(), bottomComponent[Double], bottomComponent[String], resultSet)
    }

    override def toNodeSet(list: L): V = TypedXPathValue(Set(), bottomComponent[Double], bottomComponent[String], nodeListToSet(list))

    override def matchNodeSetValues(v: V): (L, V) = (v.nodeSet, TypedXPathValue(v.bool, v.num, v.str, xmlDom.bottomList))

    /** A node-set is converted to a string by returning the string-value of the node in the node-set that is
      * first in document order. If the node-set is empty, an empty string is returned.
      */
    def nodeSetToStringValue(nodeSet: L): Option[Set[String]]

    /** Turn a node list into a set by sorting nodes in document order and removing duplicate nodes */
    def nodeListToSet(list: L): L
  }
}
