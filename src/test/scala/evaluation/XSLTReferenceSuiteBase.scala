package evaluation

import javax.xml.transform.TransformerException

import data.TestData
import org.scalatest.FunSuite
import util.EvaluationError
import xml.XMLRoot

import scala.xml.Elem

abstract class XSLTReferenceSuiteBase extends FunSuite {
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

  test("<xsl:attribute> #1") {
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

  test("<xsl:attribute> #2") {
    val xslt =
      <xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
        <xsl:template match='/'>
          <result>
            <elem1><xsl:call-template name="add-attr"/></elem1>
            <elem2 attr="foo">
              <!-- Overwrite predefined (literal) attribute -->
              <xsl:call-template name="add-attr"/>
            </elem2>
            <elem3>
              <!-- Add attribute twice (second overwrites first) -->
              <xsl:call-template name="add-attr"/>
              <xsl:call-template name="add-attr"/>
            </elem3>
            <elem4>
              <child/>
              <!-- Attribute should be ignored after child has already been added -->
              <xsl:call-template name="add-attr"/>
            </elem4>
          </result>
        </xsl:template>
        <xsl:template name="add-attr">
          <xsl:attribute name="attr">value</xsl:attribute>
        </xsl:template>
      </xsl:stylesheet>

    val data = <foo/>
    assertTransformMatches(xslt, data)
  }

  test("Multiple attributes") {
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

  test("Variables (simple)") {
    val xslt =
      <xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
        <xsl:template match='/'>
          <xsl:variable name="bool" select="true()"/>
          <result>
            <xsl:value-of select="$bool"/>
          </result>
        </xsl:template>
      </xsl:stylesheet>

    val data = <foo/>

    assertTransformMatches(xslt, data)
  }

  test("Variables (text values)") {
    val xslt =
      <xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
        <xsl:template match='/'>
          <xsl:variable name="var1"/> <!-- missing select attribute means empty string value ('') -->
          <xsl:variable name="var2" select="'hello'"/>
          <xsl:variable name="var3" select="'world'"/>
          <result>
            <xsl:value-of select="$var1"/><xsl:value-of select="$var2"/><xsl:value-of select="$var3"/>
          </result>
        </xsl:template>
      </xsl:stylesheet>

    val data = <foo/>

    assertTransformMatches(xslt, data)
  }

  test("Variable shadowing (allowed)") {
    val xslt =
      <xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
        <xsl:template match='/'>
          <xsl:variable name="bool" select="true()"/>
          <result>
            <xsl:variable name="bool" select="not($bool)"/>
            <xsl:value-of select="$bool"/>
          </result>
        </xsl:template>
      </xsl:stylesheet>

    val data = <foo/>

    assertTransformMatches(xslt, data)
  }

  test("Variable shadowing (forbidden)") {
    val xslt =
      <xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
        <xsl:template match='/'>
          <!-- Shadowing is forbidden in the same scope -->
          <xsl:variable name="bool" select="true()"/>
          <xsl:variable name="bool" select="false()"/>
          <result>
            <xsl:value-of select="$bool"/>
          </result>
        </xsl:template>
      </xsl:stylesheet>

    val data = <foo/>

    assertTransformMatches(xslt, data)
  }

  test("Copy-of node-set") {
    val xslt =
      <xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
        <xsl:template match='/root'>
          <result>
            <xsl:copy-of select="a/b"/>
          </result>
        </xsl:template>
      </xsl:stylesheet>

    val data =
      <root>
        <a>
          <b>
          </b>
        </a>
        <a>
          <b>
          </b>
        </a>
      </root>

    assertTransformMatches(xslt, data)
  }

  test("Node-set Variable") {
    val xslt =
      <xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
        <xsl:template match='/root'>
          <xsl:variable name="nodes" select="a"/>
          <result>
            <xsl:copy-of select="$nodes/b"/>
          </result>
        </xsl:template>
      </xsl:stylesheet>

    val data =
      <root>
        <a>
          <b/>
        </a>
        <a>
          <b/>
        </a>
      </root>

    assertTransformMatches(xslt, data)
  }

  test("Template parameter") {
    val xslt =
      <xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
        <xsl:template match="/">
          <result>
            <xsl:apply-templates/>
          </result>
        </xsl:template>
        <xsl:template match='a'>
          <xsl:param name="nr" select="@nr"/>
          <aa>
            <xsl:attribute name="nr"><xsl:value-of select="$nr"/></xsl:attribute>
            <xsl:apply-templates>
              <xsl:with-param name="nr" select="-1"/>
            </xsl:apply-templates>
          </aa>
        </xsl:template>
      </xsl:stylesheet>

    val data =
      <root>
        <a nr="1">
          <a/>
        </a>
        <a nr="2">
          <a/>
        </a>
      </root>

    assertTransformMatches(xslt, data)
  }

  test("Named template example") {
    assertTransformMatches(TestData.NamedTemplateExampleStylesheet, TestData.NamedTemplateExampleData)
  }

  test("Simple if") {
    val xslt =
      <xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
        <xsl:template match="/">
          <result>
            <xsl:apply-templates/>
          </result>
        </xsl:template>
        <xsl:template match='a'>
          <xsl:if test="string(@take) = 'yes'">
            <aa><xsl:apply-templates/></aa>
          </xsl:if>
        </xsl:template>
        <xsl:template match='b'>
          <bb><xsl:apply-templates/></bb>
        </xsl:template>
      </xsl:stylesheet>

    val data =
      <root>
        <a take="yes">
          <b/>
          <a take="no"/>
          <a take="yes"><b/></a>
        </a>
        <a take="no">
          <a take="yes"/>
        </a>
      </root>

    assertTransformMatches(xslt, data)
  }

  test("<xsl:choose>") {
    val xslt =
      <xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
        <xsl:template match="/root">
          <result>
            <xsl:call-template name="choose">
              <xsl:with-param name="input" select="1"/>
            </xsl:call-template>
            <xsl:call-template name="choose">
              <xsl:with-param name="input" select="2"/>
            </xsl:call-template>
            <xsl:call-template name="choose">
              <xsl:with-param name="input" select="3"/>
            </xsl:call-template>
          </result>
        </xsl:template>
        <xsl:template name='choose'>
          <xsl:param name="input" select="-1"/>
          <xsl:choose>
            <xsl:when test="$input &lt; 2">
              <lessThanTwo><xsl:value-of select="$input"/></lessThanTwo>
            </xsl:when>
            <xsl:when test="$input = 2">
              <equalsTwo><xsl:value-of select="$input"/></equalsTwo>
            </xsl:when>
            <xsl:otherwise>
              <largerThanTwo><xsl:value-of select="$input"/></largerThanTwo>
            </xsl:otherwise>
          </xsl:choose>
        </xsl:template>
      </xsl:stylesheet>

    val data =
      <root>
        <a recurse="yes">
          <b/>
          <a recurse="no"><b/></a>
          <a recurse="yes"><c/><b/></a>
          <c/>
        </a>
        <a recurse="no">
          <a/>
        </a>
        <c/>
      </root>

    assertTransformMatches(xslt, data)
  }

  test("<xsl:choose> with node names") {
    val xslt =
      <xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
        <xsl:template match="/root">
          <result>
            <xsl:apply-templates/>
          </result>
        </xsl:template>
        <xsl:template match='*'>
          <xsl:choose>
            <xsl:when test="name() = 'a' and string(@recurse) = 'yes'">
              <aa><xsl:apply-templates/></aa>
            </xsl:when>
            <xsl:when test="name() = 'a'">
              <aa/>
            </xsl:when>
            <xsl:when test="name() = 'b'">
              <bb/>
            </xsl:when>
            <xsl:otherwise>
              <otherwise/>
            </xsl:otherwise>
          </xsl:choose>
        </xsl:template>
      </xsl:stylesheet>

    val data =
      <root>
        <a recurse="yes">
          <b/>
          <a recurse="no"><b/></a>
          <a recurse="yes"><c/><b/></a>
          <c/>
        </a>
        <a recurse="no">
          <a/>
        </a>
        <c/>
      </root>

    assertTransformMatches(xslt, data)
  }

  test("No dynamic scoping (apply-templates)") {
    val xslt =
      <xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
        <xsl:template match="/">
          <result>
            <xsl:variable name="var" select="1"/>
            <xsl:apply-templates/>
          </result>
        </xsl:template>
        <xsl:template match="a">
          <xsl:value-of select="$var"/>
        </xsl:template>
      </xsl:stylesheet>

    val data = <root><a/></root>

    assertTransformMatches(xslt, data)
  }

  test("No dynamic scoping (call-template)") {
    val xslt =
      <xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
        <xsl:template match="/">
          <result>
            <xsl:variable name="var" select="1"/>
            <xsl:call-template name="callme"/>
          </result>
        </xsl:template>
        <xsl:template name="callme">
          <xsl:value-of select="$var"/>
        </xsl:template>
      </xsl:stylesheet>

    val data = <root/>

    assertTransformMatches(xslt, data)
  }

  test("position() and last()") {
    val xslt =
      <xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
        <xsl:template match="/root">
          <result>
            <unfiltered>
              <!-- Apply templates to all children -->
              <xsl:apply-templates/>
            </unfiltered>
            <filtered>
              <!-- Apply templates to 'a' children -->
              <xsl:apply-templates select="a"/>
            </filtered>
          </result>
        </xsl:template>
        <xsl:template match="a">
          <aa>
            <xsl:attribute name="pos"><xsl:value-of select="position()"/></xsl:attribute>
            <xsl:attribute name="last"><xsl:value-of select="last()"/></xsl:attribute>
          </aa>
        </xsl:template>
      </xsl:stylesheet>

    val data =
      <root>
        <a/>
        <b/>
        <b/>
        <b/>
        <a/>
      </root>

    assertTransformMatches(xslt, data)
  }

  test("count()") {
    val xslt =
      <xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
        <xsl:template match="/root">
          <result>
            <!-- Count the number of 'b' children -->
            <xsl:attribute name="b-count"><xsl:value-of select="count(b)"/></xsl:attribute>
          </result>
        </xsl:template>
      </xsl:stylesheet>

    val data =
      <root>
        <a/>
        <b/>
        <b/>
        <b/>
        <a/>
      </root>

    assertTransformMatches(xslt, data)
  }

  test("Node names") {
    val xslt =
      <xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
        <xsl:template match="/root">
          <result>
            <xsl:apply-templates/>
          </result>
        </xsl:template>
        <xsl:template match="text()"/> <!-- Ignore text nodes -->
        <xsl:template match="@*"> <!-- All attribute nodes -->
          <attribute><xsl:attribute name="name"><xsl:value-of select="name()"/></xsl:attribute></attribute>
        </xsl:template>
        <xsl:template match="*"> <!-- All element nodes -->
          <element><xsl:attribute name="name"><xsl:value-of select="name()"/></xsl:attribute></element>
        </xsl:template>
      </xsl:stylesheet>

    val data =
      <root x="foo" y="bar">
        <a/>
        <b/>
        <b/>
        <b/>
        <a/>
      </root>

    assertTransformMatches(xslt, data)
  }

  test("Copy root node") {
    val xslt =
      <xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
        <xsl:template match="/">
          <result>
            <xsl:copy-of select="/"/>
          </result>
        </xsl:template>
      </xsl:stylesheet>

    val data =
      <root>
        <a/>
        <b/>
        <c/>
      </root>

    assertTransformMatches(xslt, data)
  }

  test("Replicate n times") {
    val xslt =
      <xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
        <xsl:template match="/copy">
          <result>
            <xsl:apply-templates>
              <xsl:with-param name="n" select="number(@n)"/>
            </xsl:apply-templates>
          </result>
        </xsl:template>
        <xsl:template match="*">
          <xsl:param name="n" select="0"/>
          <xsl:call-template name="copy">
            <xsl:with-param name="n" select="$n"/>
          </xsl:call-template>
        </xsl:template>
        <xsl:template name="copy">
          <xsl:param name="n" select="0"/>
          <xsl:if test="$n > 0">
            <xsl:copy-of select="."/>
            <xsl:call-template name="copy">
              <xsl:with-param name="n" select="$n - 1"/>
            </xsl:call-template>
          </xsl:if>
        </xsl:template>
      </xsl:stylesheet>

    val data =
      <copy n="5">
        <one foo="bar"><foo/></one>
        <two/>
      </copy>

    assertTransformMatches(xslt, data)
  }

  test("Wrong types for union operator") {
    val xslt =
      <xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
        <xsl:template match="/">
          <result>
            <xsl:variable name="x" select="1"/>
            <xsl:value-of select="$x | $x"/>
          </result>
        </xsl:template>
      </xsl:stylesheet>

    val data =
        <root/>

    assertTransformMatches(xslt, data)
  }

  test("Choose with error") {
    val xslt =
      <xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
        <xsl:template match="/">
          <result>
            <xsl:choose>
              <xsl:when test="1 = 0"><false/></xsl:when>
              <!--<xsl:when test=""><impossible/></xsl:when>-->
              <xsl:when test="0 = 0"><true><xsl:variable name="x" select="1"/><xsl:value-of select="$x | $x"/></true></xsl:when>
              <xsl:otherwise><otherwise/></xsl:otherwise>
            </xsl:choose>
          </result>
        </xsl:template>
      </xsl:stylesheet>

    val data =
      <root/>

    assertTransformMatches(xslt, data)
  }

  test("Division by zero") {
    val xslt =
      <xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
        <xsl:template match="/">
          <result>
            <div>
              <xsl:value-of select="1 div 0"/>
            </div>
            <mod>
              <xsl:value-of select="1 mod 0"/>
            </mod>
          </result>
        </xsl:template>
      </xsl:stylesheet>

    val data =
        <root/>

    assertTransformMatches(xslt, data)
  }

  test("node() test") {
    val xslt =
      <xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
        <xsl:template match="/root">
          <result>
            <childaxis>
              <xsl:apply-templates select="node()"/>
            </childaxis>
            <attributeaxis>
              <xsl:apply-templates select="attribute::node()"/>
            </attributeaxis>
          </result>
        </xsl:template>
        <xsl:template match="node()">
          <xsl:copy-of select="."/>
        </xsl:template>
      </xsl:stylesheet>

    // The handling of comment nodes is weird, therefore we don't test them here (TODO: remove comment nodes completely?)
    val data =
        <root attr1="val1" attr2="val2"><elem/>Some Text</root>

    assertTransformMatches(xslt, data)
  }

  def transform(xslt: Elem, data: Elem): XMLRoot

  def assertTransformMatches(xslt: Elem, data: Elem) = {
    try {
      val referenceResult = TransformHelper.transformJava(xslt, data)
      println(referenceResult)
      assertResult(referenceResult) { transform(xslt, data) }
    } catch {
      case eJava: TransformerException =>
        // if Java throws an exception, we should do the same (because of invalid input)
        val eScala = intercept[EvaluationError] (transform(xslt, data))
        println(f"Scala error: $eScala")
        println(f"Java error: $eJava")
    }
  }
}
