package evaluation

import java.io.{StringReader, StringWriter}
import javax.xml.transform.stream.{StreamResult, StreamSource}
import javax.xml.transform.{OutputKeys, TransformerFactory}

import analysis.domain.Domain
import analysis.domain.concrete._
import analysis.domain.powerset.{PowersetXMLDomain, PowersetDomain}
import analysis.XSLTAnalyzer
import util.EvaluationError
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
    val liftedInput: PowersetXMLDomain.N = Right(Set(XMLParser.parseDocument(data)))
    analyzer.transform(stylesheet, liftedInput) match {
      case Left(_) => throw new AssertionError("Expected single result root element, but got infinite result (TOP)")
      case Right(s) => s.toList match {
        case List(r: XMLRoot) => r
        case Nil => throw new EvaluationError("Expected single result root element, but got no result (BOTTOM)")
        case _ => throw new AssertionError(f"Expected single result root element, but got $s")
      }
    }
  }

  def transformAbstractConcrete(xslt: Elem, data: Elem): XMLRoot = {
    val stylesheet = XSLTParser.parseStylesheet(xslt)
    val analyzer = new XSLTAnalyzer(ConcreteDomain)
    val xmlDom = analyzer.xmlDom
    val liftedInput: ConcreteXMLDomain.N = Value(XMLParser.parseDocument(data))
    val result = analyzer.transform(stylesheet, liftedInput)
    result match {
      case Top => throw new AssertionError("Expected single result root element, but got infinite result (TOP)")
      case Value(r: XMLRoot) => r
      case Bottom => throw new EvaluationError("Expected single result root element, but got no result (BOTTOM)")
      case _ => throw new AssertionError(f"Expected single result root element, but got $result")
    }
  }

  def transformAbstract[N, L, V](xslt: Elem, data: N, domain: Domain[N, L, V], disableBuiltinTemplates: Boolean = false): N = {
    val stylesheet = XSLTParser.parseStylesheet(xslt, disableBuiltinTemplates)
    val analyzer = new XSLTAnalyzer(domain)
    analyzer.transform(stylesheet, data)
  }
}
