package tests.reference

import javax.xml.transform.TransformerException

import analysis.domain.XMLParser
import analysis.domain.zipper.ZipperDomain
import analysis.domain.zipper.ZipperXMLDomain.N
import tests.TransformHelper
import util.ProcessingError

import scala.xml.Elem

class XSLTZipperReferenceSuite extends XSLTReferenceSuiteBase[N] {
  val parser = new XMLParser(ZipperDomain)

  override def transform(xslt: Elem, data: Elem): N = {
    val parsedData = parser.parseDocument(data)
    TransformHelper.transformAbstract(xslt, parsedData, ZipperDomain)
  }

  override def checkMatch(transformed: Either[N, ProcessingError], referenceResult: Either[Elem, TransformerException]) = {
    // use BOTTOM instead of exceptions
    val parsedReference: N = referenceResult match {
      case Left(res) => parser.parseDocument(res)
      case Right(_) => ZipperDomain.xmlDom.bottom
    }

    val transformedResult: N = transformed match {
      case Left(res) => res
      case Right(res) => ZipperDomain.xmlDom.bottom
    }

    // this checks only that the actual result is a superset of the expected reference result, not an exact match
    // because our domain is too coarse to guarantee that
    assert(ZipperDomain.xmlDom.lessThanOrEqual(parsedReference, transformedResult), f"Result: ${transformedResult._1}; Expected: ${parsedReference._1}")
    println(transformedResult._1)
  }
}
