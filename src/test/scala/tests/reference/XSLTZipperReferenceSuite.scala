package tests.reference

import analysis.domain.XMLParser
import analysis.domain.zipper.ZipperDomain
import analysis.domain.zipper.ZipperXMLDomain.N
import tests.TransformHelper
import util.EvaluationError

import scala.xml.Elem

class XSLTZipperReferenceSuite extends XSLTReferenceSuiteBase[N] {
  val parser = new XMLParser(ZipperDomain)

  override def transform(xslt: Elem, data: Elem): N = {
    val parsedData = parser.parseDocument(data)
    val result = TransformHelper.transformAbstract(xslt, parsedData, ZipperDomain)
    if (ZipperDomain.xmlDom.lessThanOrEqual(result, ZipperDomain.xmlDom.bottom)) {
      throw new EvaluationError("BOTTOM was returned.") // the test suite expects an EvaluationError for any invalid input
    } else {
      result
    }
  }

  override def checkMatch(transformed: N, referenceResult: Elem) = {
    val parsedReference = parser.parseDocument(referenceResult)
    assert(ZipperDomain.xmlDom.lessThanOrEqual(parsedReference, transformed), f"Result: ${transformed._1}; Expected: ${parsedReference._1}")
    println(transformed._1)
  }
}
