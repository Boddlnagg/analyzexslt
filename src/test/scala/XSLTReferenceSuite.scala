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

  test("Match attribute #1") {
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

  test("Match attribute #2") {
    val xslt =
      <xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
        <xsl:template match="/root">
          <xsl:apply-templates select="elem/@attr"/>
        </xsl:template>
        <xsl:template match="@attr">
          <result>
            <xsl:apply-templates/> <!-- This should not match any children, because attribute nodes don't have children -->
          </result>
        </xsl:template>
        <xsl:template match="@*|*"><match/></xsl:template>
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

  test("Literal elements") {
    val xslt =
      <xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
        <xsl:template match='/'>
          <outer><inner attr="test"/></outer>
        </xsl:template>
      </xsl:stylesheet>

    val data = <foo/>
    assertTransformMatches(xslt, data)
  }

  test("Literal elements and text nodes") {
    val xslt =
      <xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
        <xsl:template match='/'>
          <p>text before<br/> text after</p>
        </xsl:template>
      </xsl:stylesheet>

    val data = <foo/>
    assertTransformMatches(xslt, data)
  }

  test("<xsl:attribute>") {
    val xslt =
      <xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
        <xsl:template match='/'>
          <p>
            <xsl:attribute name="attr">
              attr<p>-</p>value
            </xsl:attribute>
            text before<br/> text after</p>
        </xsl:template>
      </xsl:stylesheet>

    val data = <foo/>
    assertTransformMatches(xslt, data)
  }

  test("Overwrite attribute") {
    val xslt =
      <xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
        <xsl:template match='/'>
          <p attr="OLD">
            <!-- This should overwrite the attribute in the literal so that the value will be 'NEW' -->
            <xsl:attribute name="attr">NEW</xsl:attribute>
          </p>
        </xsl:template>
      </xsl:stylesheet>

    val data = <foo/>
    assertTransformMatches(xslt, data)
  }

  test("Set attribute after element") {
    val xslt =
      <xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
        <xsl:template match='/'>
          <p attr="OLD">
            <element/> <!-- This element invalidates the the following xsl:attribute, so that will be ignored -->
            <xsl:attribute name="attr">NEW</xsl:attribute>
          </p>
        </xsl:template>
      </xsl:stylesheet>

    val data = <foo/>
    assertTransformMatches(xslt, data)
  }

  test("Nested templates (without select)") {
    val xslt =
      <xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
        <xsl:template match='a'>
          <aa>
            <xsl:apply-templates/>
          </aa>
        </xsl:template>
        <xsl:template match='b'>
          <bb>
            <xsl:apply-templates/>
          </bb>
        </xsl:template>
        <xsl:template match='c'>
          <cc/> <!-- No recursive application -->
        </xsl:template>
      </xsl:stylesheet>

    // NOTE: no nice tree formatting here so we don't create text nodes, which would trigger the built-in rule to copy them
    val data = <a><a><b/></a><b><b><a/><a/></b><c><a/></c></b></a>

    assertTransformMatches(xslt, data)
  }

  def assertTransformMatches(xslt: Elem, data: Elem) = {
    assertResult(TransformHelper.transformJava(xslt, data)) { TransformHelper.transformScala(xslt, data) }
  }
}
