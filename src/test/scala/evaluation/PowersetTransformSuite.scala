package evaluation

import analysis.domain.powerset.{PowersetXMLDomain, PowersetDomain}
import xml.XMLParser
import org.scalatest.FunSuite

import scala.xml.Elem

class PowersetTransformSuite extends FunSuite {
  val xmlDom = PowersetDomain.xmlDom
  val xpathDom = PowersetDomain.xpathDom

  def transform(xslt: Elem, source: PowersetXMLDomain.N = xmlDom.top) =
    TransformHelper.transformAbstract(xslt: Elem, source, PowersetDomain, true)

  test("Simple literal result element") {
    val xslt =
      <xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
        <xsl:template match='/'>
          <result/>
        </xsl:template>
      </xsl:stylesheet>

    assertResult(Some(Set(XMLParser.parseDocument(<result/>)))) { transform(xslt) }
  }

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

    assertResult(Some(Set(XMLParser.parseDocument(<result><child/></result>)))) { transform(xslt) }
  }
}
