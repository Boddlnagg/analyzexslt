package evaluation

import java.io.{StringReader, StringWriter}
import javax.xml.transform.stream.{StreamResult, StreamSource}
import javax.xml.transform.{OutputKeys, TransformerFactory}

import analysis.domain.powerset.{PowersetXPathXMLDomain, PowersetXPathDomain, PowersetXMLDomain}
import analysis.{XPathAnalyzer, XSLTAnalyzer}
import xml.{XMLElement, XMLParser, XMLRoot}
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

  object PowersetXPathAnalyzer extends XPathAnalyzer[PowersetXMLDomain.N, PowersetXMLDomain.L, PowersetXPathDomain.V, PowersetXMLDomain.D.type, PowersetXPathXMLDomain.type] {
    val xmlDom = PowersetXMLDomain.D
    val xpathDom = PowersetXPathXMLDomain
  }

  object PowersetXSLTAnalyzer extends XSLTAnalyzer[PowersetXMLDomain.N, PowersetXMLDomain.L, PowersetXPathDomain.V, PowersetXMLDomain.D.type, PowersetXPathXMLDomain.type] {
    override val xmlDom = PowersetXMLDomain.D
    override val xpathDom = PowersetXPathXMLDomain
    override val xpathAnalyzer = PowersetXPathAnalyzer
  }

  def transformAbstractPowerset(xslt: Elem, data: Elem): XMLRoot = {
    val stylesheet = XSLTParser.parseStylesheet(xslt)
    val xmlDom = PowersetXSLTAnalyzer.xmlDom
    val result = PowersetXSLTAnalyzer.transform(stylesheet, xmlDom.lift(XMLParser.parseDocument(data))).map(_.toList)
    result match {
      case None => throw new AssertionError(f"Expected single result root element, but got infinite result (TOP)")
      case Some(List(r: XMLRoot)) => r
      case _ => throw new AssertionError(f"Expected single result root element, got $result")
    }
  }
}
