package evaluation

import xml.XMLRoot

import scala.xml.Elem

class XSLTTypedPowersetReferenceSuite extends XSLTReferenceSuiteBase[XMLRoot] {
  override def transform(xslt: Elem, data: Elem): XMLRoot = TransformHelper.transformAbstractTypedPowerset(xslt, data)
}
