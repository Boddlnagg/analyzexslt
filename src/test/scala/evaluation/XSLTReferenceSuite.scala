package evaluation

import xml.{XMLParser, XMLRoot}
import xslt.{XSLTEvaluator, XSLTParser}

import scala.xml.Elem

class XSLTReferenceSuite extends XSLTReferenceSuiteBase {
  override def transform(xslt: Elem, data: Elem): XMLRoot = {
    val stylesheet = XSLTParser.parseStylesheet(xslt)
    XSLTEvaluator.transform(stylesheet, XMLParser.parseDocument(data))
  }
}
