import java.io.{StringReader, StringWriter}
import javax.xml.transform.{OutputKeys, TransformerFactory}
import javax.xml.transform.stream.{StreamResult, StreamSource}

import scala.xml.{XML, Elem}

object TransformHelper {
  def transformScala(xslt: Elem, data: Elem): XMLRoot = {
    val stylesheet = new XSLTStylesheet(xslt)
    stylesheet.transform(XMLRoot(data))
  }

  def transformJava(xslt: Elem, data: Elem): XMLRoot = {
    // this is a wrapper around the javax.xml.transform interface
    val xmlResultResource = new StringWriter()
    val xmlTransformer = TransformerFactory.newInstance().newTransformer(
      new StreamSource(new StringReader(xslt.toString))
    )
    xmlTransformer.setOutputProperty(OutputKeys.METHOD, "xml")
    xmlTransformer.transform(
      new StreamSource(new StringReader(data.toString)), new StreamResult(xmlResultResource)
    )
    XMLRoot(XML.loadString(xmlResultResource.getBuffer.toString))
  }
}
