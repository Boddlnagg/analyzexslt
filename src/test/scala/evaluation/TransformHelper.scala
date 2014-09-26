package evaluation

import java.io.{StringReader, StringWriter}
import javax.xml.transform.stream.{StreamResult, StreamSource}
import javax.xml.transform.{OutputKeys, TransformerFactory}

import analysis.domain.Domain
import analysis.domain.powerset.PowersetDomain
import analysis.XSLTAnalyzer
import xml.{XMLParser, XMLRoot}
import xslt.{XSLTEvaluator, XSLTParser}

import scala.xml.Elem


object TransformHelper {
  def transformScala(xslt: Elem, data: Elem): XMLRoot = {
    val stylesheet = XSLTParser.parseStylesheet(xslt)
    XSLTEvaluator.transform(stylesheet, XMLParser.parseDocument(data))
  }

  def transformJava(xslt: Elem, data: Elem): XMLRoot = {
    // this is a wrapper around the javax.xml.transform interface
    val xmlResultResource = new StringWriter()
    val xmlTransformer = TransformerFactory.newInstance().newTransformer(
      new StreamSource(new StringReader(xslt.toString()))
    )
    xmlTransformer.setOutputProperty(OutputKeys.METHOD, "xml")
    xmlTransformer.transform(
      new StreamSource(new StringReader(data.toString())), new StreamResult(xmlResultResource)
    )
    XMLParser.parseDocument(xmlResultResource.getBuffer.toString)
  }

  def transformAbstractPowerset(xslt: Elem, data: Elem): XMLRoot = {
    val stylesheet = XSLTParser.parseStylesheet(xslt)
    val analyzer = new XSLTAnalyzer(PowersetDomain)
    val xmlDom = analyzer.xmlDom
    val result = analyzer.transform(stylesheet, analyzer.xmlDom.liftDocument(XMLParser.parseDocument(data))).map(_.toList)
    result match {
      case None => throw new AssertionError(f"Expected single result root element, but got infinite result (TOP)")
      case Some(List(r: XMLRoot)) => r
      case _ => throw new AssertionError(f"Expected single result root element, got $result")
    }
  }

  def transformAbstract[N, L, V](xslt: Elem, data: N, domain: Domain[N, L, V]): N = {
    val stylesheet = XSLTParser.parseStylesheet(xslt)
    val analyzer = new XSLTAnalyzer(domain)
    analyzer.transform(stylesheet, data)
  }
}
