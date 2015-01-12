package tests.reference

import tests.TransformHelper
import xml.XMLRoot

import scala.xml.Elem

class XSLTConcreteReferenceSuite extends XSLTReferenceSuiteBase[XMLRoot] {
  override def transform(xslt: Elem, data: Elem): XMLRoot = TransformHelper.transformAbstractConcrete(xslt, data)
}
