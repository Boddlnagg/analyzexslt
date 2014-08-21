import org.scalatest.FunSuite
import scala.xml.Elem

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

  test("Attribute match") {
    val xslt =
      <xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
        <xsl:template match="/root">
          <xsl:apply-templates select="elem/@attr"/>
        </xsl:template>
        <xsl:template match="@attr">
          <result><xsl:value-of select="."/></result>
        </xsl:template>
      </xsl:stylesheet>
    val data = <root><elem attr="value"/></root>

    assertTransformMatches(xslt, data)
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
    assertResult(TransformHelper.transformJava(xslt, data)) { TransformHelper.transformScala(xslt, data) }
  }
}
