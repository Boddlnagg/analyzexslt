package tests.reference

import tests.TransformHelper
import xml.XMLRoot

import scala.xml.Elem

/** This runs the reference suite using the typed powerset domain. It checks that the resulting set
  * is a singleton set containing exactly the reference result.
  */
class XSLTTypedPowersetReferenceSuite extends XSLTReferenceSuiteBase[XMLRoot] {
  override def transform(xslt: Elem, data: Elem): XMLRoot = TransformHelper.transformAbstractTypedPowerset(xslt, data)
}
