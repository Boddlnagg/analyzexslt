package evaluation

import analysis.domain.XMLParser
import analysis.domain.zipper.ZipperDomain
import analysis.domain.zipper.ZipperXMLDomain.N
import scala.xml.Elem

class XSLTZipperReferenceSuite extends XSLTReferenceSuiteBase[N] {
  val parser = new XMLParser(ZipperDomain)

  override def transform(xslt: Elem, data: Elem): N = {
    val parsedData = parser.parseDocument(data)
    TransformHelper.transformAbstract(xslt, parsedData, ZipperDomain)
  }

  override def checkMatch(transformed: N, referenceResult: Elem) = {
    val parsedReference = parser.parseDocument(referenceResult)
    assert(ZipperDomain.xmlDom.lessThanOrEqual(parsedReference, transformed), f"Result: $transformed, expected $parsedReference")
  }

  // TODO: override checkMatch to check for <= instead of ==
}
