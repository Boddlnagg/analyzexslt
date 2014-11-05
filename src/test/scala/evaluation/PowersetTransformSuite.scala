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

  test("No templates") {
    val xslt =
      <xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
      </xsl:stylesheet>

    // built-in templates are disabled, so this should evaluate to BOTTOM
    assertResult(Some(Set())) { transform(xslt) }
  }

  /*test("No matching templates") {
    val xslt =
      <xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
        <xsl:template match="/foo"><bar/></xsl:template>
      </xsl:stylesheet>

    // no template matches the root node (/), because built-in templates are disabled, so this should evaluate to BOTTOM
    assertResult(Some(Set())) { transform(xslt) }
  }*/

  /* TODO: this doesn't currently terminate because of endless recursion
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
  */
}