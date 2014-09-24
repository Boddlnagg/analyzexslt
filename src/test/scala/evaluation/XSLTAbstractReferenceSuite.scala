package evaluation

import xml.XMLRoot

import scala.xml.Elem

class XSLTAbstractReferenceSuite extends XSLTReferenceSuiteBase {
  override def transform(xslt: Elem, data: Elem): XMLRoot = TransformHelper.transformAbstractPowerset(xslt, data)
}
