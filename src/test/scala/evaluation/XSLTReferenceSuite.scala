package evaluation

import xml.{XMLParser, XMLRoot}
import xslt.{XSLTEvaluator, XSLTParser}

import scala.xml.Elem

class XSLTReferenceSuite extends XSLTReferenceSuiteBase[XMLRoot] {
  override def transform(xslt: Elem, data: Elem): XMLRoot = TransformHelper.transformScala(xslt, data)
}
