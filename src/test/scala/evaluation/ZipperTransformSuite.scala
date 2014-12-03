package evaluation

import analysis.domain.zipper.ZipperXMLDomain._
import analysis.domain.zipper._
import analysis.domain.XMLParser
import data.TestData
import org.scalatest.FunSuite

import scala.xml.Elem

class ZipperTransformSuite extends FunSuite {
  val xmlDom = ZipperDomain.xmlDom
  val xpathDom = ZipperDomain.xpathDom
  val parser = new XMLParser(ZipperDomain)

  def transform(xslt: Elem, source: ZipperXMLDomain.N = xmlDom.top) =
    TransformHelper.transformAbstract(xslt: Elem, source, ZipperDomain, true)

  test("Simple literal result element") {
    val xslt =
      <xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
        <xsl:template match='/'>
          <result/>
        </xsl:template>
      </xsl:stylesheet>

    assertResult(parser.parseDocument(<result/>)) { transform(xslt) }
  }

  test("No templates") {
    val xslt =
      <xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
      </xsl:stylesheet>

    // built-in templates are disabled, so this should evaluate to BOTTOM
    assertResult(xmlDom.bottom) { transform(xslt) }
  }

  test("No matching template (for /)") {
    val xslt =
      <xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
        <xsl:template match="/foo"><bar/></xsl:template>
      </xsl:stylesheet>

    // no template matches the root node (/), because built-in templates are disabled, so this should evaluate to BOTTOM
    assertResult(xmlDom.bottom) { transform(xslt) }
  }

  // TODO: fix this test
  /*test("Wikipedia (XSLT #2 simplified)") {
    assertResult(???) { transform(TestData.WikipediaStylesheet2) }
  }*/

  test("Simple recursive template") {
    val xslt =
      <xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
        <xsl:template match='/'>
          <result>
            <xsl:apply-templates/>
          </result>
        </xsl:template>
        <xsl:template match="/root">
          <child/>
        </xsl:template>
      </xsl:stylesheet>

    assertResult(parser.parseDocument(<result><child/></result>)) { transform(xslt) }
  }

  test("Match attribute") {
    val xslt =
      <xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
        <xsl:template match ="/">
            <xsl:apply-templates/>
        </xsl:template>
        <xsl:template match="/root">
          <xsl:apply-templates select="elem/@attr"/>
        </xsl:template>
        <xsl:template match="@attr">
          <result><xsl:value-of select="."/></result>
        </xsl:template>
      </xsl:stylesheet>

    assertResult((Subtree(Set(Root),Some(Set()),
      ZCons(Subtree(Set(Element("result")),Some(Set()),
        ZCons(Subtree(Set(AnyText),Some(Set()),ZNil()), ZNil())
      ), ZNil())
    ), Set(RootPath))) {
      transform(xslt)
    }
  }

  test("Multiple attributes") {
    // the output of this stylesheet is completely independent of the input
    val xslt =
      <xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
        <xsl:template match='/'>
          <result literal1="foo" literal2="bar">
            <!-- The order of these is not defined in the spec, but Java adds them in reverse order, so we should match that -->
            <xsl:attribute name="dynamic1">1</xsl:attribute>
            <xsl:attribute name="dynamic2">2</xsl:attribute>
            <xsl:attribute name="dynamic3">3</xsl:attribute>
          </result>
        </xsl:template>
      </xsl:stylesheet>

    assertResult(parser.parseDocument(<result literal1="foo" literal2="bar" dynamic1="1" dynamic2="2" dynamic3="3"/>)) {
      transform(xslt)
    }
  }
}
