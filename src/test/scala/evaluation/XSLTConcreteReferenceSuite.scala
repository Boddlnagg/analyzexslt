package evaluation

import xml.XMLRoot

import scala.xml.Elem

class XSLTConcreteReferenceSuite extends XSLTReferenceSuiteBase {
  override def transform(xslt: Elem, data: Elem): XMLRoot = TransformHelper.transformAbstractConcrete(xslt, data)
}
