package tests

import java.io.{StringReader, StringWriter}
import javax.xml.transform.stream.{StreamResult, StreamSource}
import javax.xml.transform.{OutputKeys, TransformerFactory}

import analysis.domain.Domain
import analysis.domain.concrete._
import analysis.domain.powerset.{TypedPowersetDomain, PowersetXMLDomain, PowersetDomain}
import analysis.XSLTAnalyzer
import util.ProcessingError
import xml.{XMLParser, XMLRoot}
import xslt.{XSLTProcessor, XSLTParser}

import scala.xml.{XML, Elem}

object TransformHelper {
  /** This is a helper function for transforming a concrete document
    * with our implementation of a concrete interpreter.
    * The abstract interpreter is not involved.
    */
  def transformScala(xslt: Elem, data: Elem): XMLRoot = {
    val stylesheet = XSLTParser.parseStylesheet(xslt)
    XSLTProcessor.transform(stylesheet, XMLParser.parseDocument(data))
  }

  /** This is a helper function for transforming a concrete document
    * with the Java built-in transformer.
    * It is used to generate reference results for tests.
    */
  def transformJava(xslt: Elem, data: Elem): Elem = {
    // this is a wrapper around the javax.xml.transform interface
    val xmlResultResource = new StringWriter()
    val xmlTransformer = TransformerFactory.newInstance().newTransformer(
      new StreamSource(new StringReader(xslt.toString()))
    )
    xmlTransformer.setOutputProperty(OutputKeys.METHOD, "xml")
    xmlTransformer.transform(
      new StreamSource(new StringReader(data.toString())), new StreamResult(xmlResultResource)
    )
    // NOTE: XML.loadString ignores XML comments completely, therefore transformations that
    //       use <xsl:comment> won't return the correct result here
    XML.loadString(xmlResultResource.getBuffer.toString)
  }

  /** This is a helper function for transforming a concrete document
    * with the abstract interpreter, using the powerset domain.
    */
  def transformAbstractPowerset(xslt: Elem, data: Elem): XMLRoot = {
    val stylesheet = XSLTParser.parseStylesheet(xslt)
    val analyzer = new XSLTAnalyzer(PowersetDomain)
    val xmlDom = analyzer.xmlDom
    val liftedInput: PowersetXMLDomain.N = Some(Set(XMLParser.parseDocument(data)))
    val result = analyzer.transform(stylesheet, liftedInput).map(_.toList)
    result match {
      case None => throw new AssertionError("Expected single result root element, but got infinite result (TOP)")
      case Some(List(r: XMLRoot)) => r
      case Some(Nil) => throw new ProcessingError("Expected single result root element, but got no result (BOTTOM)")
      case _ => throw new AssertionError(f"Expected single result root element, but got $result")
    }
  }

  /** This is a helper function for transforming a concrete document
    * with the abstract interpreter, using the typed powerset domain.
    */
  def transformAbstractTypedPowerset(xslt: Elem, data: Elem): XMLRoot = {
    val stylesheet = XSLTParser.parseStylesheet(xslt)
    val analyzer = new XSLTAnalyzer(TypedPowersetDomain)
    val xmlDom = analyzer.xmlDom
    val liftedInput: PowersetXMLDomain.N = Some(Set(XMLParser.parseDocument(data)))
    val result = analyzer.transform(stylesheet, liftedInput).map(_.toList)
    result match {
      case None => throw new AssertionError("Expected single result root element, but got infinite result (TOP)")
      case Some(List(r: XMLRoot)) => r
      case Some(Nil) => throw new ProcessingError("Expected single result root element, but got no result (BOTTOM)")
      case _ => throw new AssertionError(f"Expected single result root element, but got $result")
    }
  }

  /** This is a helper function for transforming a concrete document
    * with the abstract interpreter, using the concrete domain.
    */
  def transformAbstractConcrete(xslt: Elem, data: Elem): XMLRoot = {
    val stylesheet = XSLTParser.parseStylesheet(xslt)
    val analyzer = new XSLTAnalyzer(ConcreteDomain)
    val xmlDom = analyzer.xmlDom
    val liftedInput: ConcreteXMLDomain.N = Value(XMLParser.parseDocument(data))
    val result = analyzer.transform(stylesheet, liftedInput)
    result match {
      case Top => throw new AssertionError("Expected single result root element, but got infinite result (TOP)")
      case Value(r: XMLRoot) => r
      case Bottom => throw new ProcessingError("Expected single result root element, but got no result (BOTTOM)")
      case _ => throw new AssertionError(f"Expected single result root element, but got $result")
    }
  }

  /** This is a helper function for transforming a concrete document
    * with the abstract interpreter, using a given domain.
    * The result is returned in the domain representation.
    */
  def transformAbstract[N, L, V](xslt: Elem, data: N, domain: Domain[N, L, V], disableBuiltinTemplates: Boolean = false, recursionLimit: Option[Int] = None): N = {
    val stylesheet = XSLTParser.parseStylesheet(xslt, disableBuiltinTemplates)
    val analyzer = new XSLTAnalyzer(domain)
    analyzer.transform(stylesheet, data, recursionLimit)
  }
}
