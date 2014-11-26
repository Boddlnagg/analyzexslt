package evaluation

import analysis.domain.XMLParser
import analysis.domain.zipper.ZipperDomain
import analysis.domain.zipper.ZipperXMLDomain.N
import scala.xml.Elem

class XSLTZipperReferenceSuite extends XSLTReferenceSuiteBase[N] {
  val parser = new XMLParser(ZipperDomain)

  override def transform(xslt: Elem, data: Elem): N = {
    val parsedData = parser.parse(data)
    TransformHelper.transformAbstract(xslt, parsedData, ZipperDomain)
  }

  // TODO: override checkMatch to check for <= instead of ==
}
