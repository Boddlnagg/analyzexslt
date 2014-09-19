import java.io.{StringReader, StringWriter}
import javax.xml.transform.{OutputKeys, TransformerFactory}
import javax.xml.transform.stream.{StreamResult, StreamSource}

import scala.xml.Elem

import xml.{XMLRoot, XMLParser}
import xslt.{XSLTEvaluator, XSLTParser}


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
}
