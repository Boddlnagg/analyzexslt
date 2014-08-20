import org.scalatest.FunSuite
import java.io.{StringReader, StringWriter}
import javax.xml.transform.{TransformerFactory, OutputKeys}
import javax.xml.transform.stream.{StreamResult, StreamSource}
import scala.xml.{XML, Elem}

class XSLTReferenceSuite extends FunSuite {
  test("Wikipedia (Java Example)") {
    // taken from http://en.wikipedia.org/wiki/Java_API_for_XML_Processing#Example
    val xslt =
      <xsl:stylesheet version='1.0' xmlns:xsl='http://www.w3.org/1999/XSL/Transform'>
        <xsl:template match='/'>
          <reRoot><reNode><xsl:value-of select='/root/node/@val' /> world</reNode></reRoot>
        </xsl:template>
      </xsl:stylesheet>

    val data = <root><node val='hello'/></root>
    assertTransformMatches(xslt, data)
  }

  test("Wikipedia (XSLT #1 simplified)") {
    assertTransformMatches(TestData.WikipediaStylesheet1, TestData.WikipediaData)
  }

  test("Wikipedia (XSLT #2 simplified)") {
    assertTransformMatches(TestData.WikipediaStylesheet2, TestData.WikipediaData)
  }

  test("Simple transform (input = output)") {
    val xslt =
      <xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
        <xsl:template match='/'>
          <foo/>
        </xsl:template>
      </xsl:stylesheet>

    val data = <foo/>
    assertTransformMatches(xslt, data)
  }

  def assertTransformMatches(xslt: Elem, data: Elem) = {
    assertResult(transformJava(xslt, data)) (transformScala(xslt, data))
  }

  def transformScala(xslt: Elem, data: Elem): XMLElement = {
    val stylesheet = new XSLTStylesheet(xslt)
    stylesheet.transform(XMLElement(data))
  }

  def transformJava(xslt: Elem, data: Elem): XMLElement = {
    // this is a wrapper around the javax.xml.transform interface
    val xmlResultResource = new StringWriter()
    val xmlTransformer = TransformerFactory.newInstance().newTransformer(
      new StreamSource(new StringReader(xslt.toString))
    )
    xmlTransformer.setOutputProperty(OutputKeys.METHOD, "xml")
    xmlTransformer.transform(
      new StreamSource(new StringReader(data.toString)), new StreamResult(xmlResultResource)
    )
    XMLElement(XML.loadString(xmlResultResource.getBuffer.toString))
  }
}
