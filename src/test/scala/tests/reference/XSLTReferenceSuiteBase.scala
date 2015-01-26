package tests.reference

import javax.xml.transform.TransformerException

import data.TestData
import org.scalatest.FunSuite
import org.xml.sax.SAXParseException
import tests.TransformHelper
import util.ProcessingError
import xml.XMLParser

import scala.xml.Elem

abstract class XSLTReferenceSuiteBase[T] extends FunSuite {
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

  test("Wikipedia (Java Example) using concat") {
    // taken from http://en.wikipedia.org/wiki/Java_API_for_XML_Processing#Example
    val xslt =
      <xsl:stylesheet version='1.0' xmlns:xsl='http://www.w3.org/1999/XSL/Transform'>
        <xsl:template match='/'>
          <reRoot><reNode><xsl:value-of select='concat(string(/root/node/@val), " ", "world")'/></reNode></reRoot>
        </xsl:template>
      </xsl:stylesheet>

    val data = <root><node val='hello'/></root>
    assertTransformMatches(xslt, data)
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

  test("No empty text node") {
    val xslt =
      <xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
        <xsl:template match="/">
          <!-- This copies the empty value of the @empty attribute to the result, which is then discarded -->
          <result><xsl:value-of select="/root/@empty"/></result>
        </xsl:template>
      </xsl:stylesheet>
    val data = <root empty=""/>

    assertTransformMatches(xslt, data)
  }

  test("Simple transform (input = output)") {
    val xslt =
      <xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
        <xsl:template match='/'>
          <foo/>
        </xsl:template>
      </xsl:stylesheet>

    val data = <foo/> // stylesheet is completely independent of input, we just need *some* document
    assertTransformMatches(xslt, data)
  }

  test("Creating elements") {
    val xslt =
      <xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
        <xsl:template match='/'>
          <outer>
            <inner attr="test"/>
            <xsl:element name="inner2"><innermost/></xsl:element>
          </outer>
        </xsl:template>
      </xsl:stylesheet>

    val data = <foo/> // stylesheet is completely independent of input, we just need *some* document
    assertTransformMatches(xslt, data)
  }

  test("Literal elements and text nodes") {
    val xslt =
      <xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
        <xsl:template match='/'>
          <p>text before<br/> text after</p>
        </xsl:template>
      </xsl:stylesheet>

    val data = <foo/> // stylesheet is completely independent of input, we just need *some* document
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

    val data = <foo/> // stylesheet is completely independent of input, we just need *some* document
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

    val data = <foo/> // stylesheet is completely independent of input, we just need *some* document
    assertTransformMatches(xslt, data)
  }

  test("Multiple attributes") {
    val xslt =
      <xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
        <xsl:template match='/'>
          <result literal1="foo" literal2="bar">
            <xsl:attribute name="dynamic1">1</xsl:attribute>
            <xsl:attribute name="dynamic2">2</xsl:attribute>
            <xsl:attribute name="dynamic3">3</xsl:attribute>
          </result>
        </xsl:template>
      </xsl:stylesheet>

    val data = <foo/> // stylesheet is completely independent of input, we just need *some* document
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

    val data = <foo/> // stylesheet is completely independent of input, we just need *some* document
    assertTransformMatches(xslt, data)
  }

  test("Set attribute after element") {
    val xslt =
      <xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
        <xsl:template match='/'>
          <p attr="OLD">
            <element/> <!-- This element invalidates the following xsl:attribute, so that will be ignored -->
            <xsl:attribute name="attr">NEW</xsl:attribute>
          </p>
        </xsl:template>
      </xsl:stylesheet>

    val data = <foo/> // stylesheet is completely independent of input, we just need *some* document
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

    val data = <foo/> // stylesheet is completely independent of input, we just need *some* document

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

    val data = <foo/> // stylesheet is completely independent of input, we just need *some* document

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

    val data = <foo/> // stylesheet is completely independent of input, we just need *some* document

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

    val data = <foo/> // stylesheet is completely independent of input, we just need *some* document

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
        <xsl:template match="text()"></xsl:template>
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
        <xsl:template match="text()"></xsl:template>
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
            <xsl:apply-templates select="@*"/>
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
      <foo/> // stylesheet is completely independent of input, we just need *some* document

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
        <foo/> // stylesheet is completely independent of input, we just need *some* document

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

    // Comment nodes are handled incorrectly in the Java reference processor, therefore we don't test them here
    val data =
        <root attr1="val1" attr2="val2"><elem/>Some Text</root>

    assertTransformMatches(xslt, data)
  }

  test("Boolean evaluation (bottom)") {
    val xslt =
      <xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
        <xsl:template match="/">
          <result>
            <xsl:variable name="x" select="1"/>
            <!-- Can't test short-circuit evaluation (`or` instead of `and`), because Java violates the spec -->
            <xsl:if test="true() and ($x | $x)">
              <foobar/>
            </xsl:if>
          </result>
        </xsl:template>
      </xsl:stylesheet>

    val data = <foo/> // stylesheet is completely independent of input, we just need *some* document

    assertTransformMatches(xslt, data)
  }

  test("Root selector from non-root context-node") {
    val xslt =
      <xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
        <xsl:template match="/">
          <result>
            <xsl:apply-templates select="//a"/> <!-- for every <a> descendant ... -->
          </result>
        </xsl:template>
        <xsl:template match="a">
          <aa>
            <xsl:apply-templates select="/root/b"/> <!-- ... create a <bb> for every /root/b -->
          </aa>
        </xsl:template>
        <xsl:template match="b">
          <bb/>
        </xsl:template>
      </xsl:stylesheet>

    val data =
      <root>
        <a><b/></a>
        <a/>
        <b/>
        <b><a/></b>
      </root>

    assertTransformMatches(xslt, data)
  }

  test("Descendant attributes") {
    val xslt =
      <xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
        <xsl:template match="/">
          <result><xsl:apply-templates select="/foo//@*"/></result>
        </xsl:template>
        <xsl:template match="foo//@*">
          <attr>
            <xsl:attribute name="name">
              <xsl:value-of select="name(.)"/>
            </xsl:attribute>
          </attr>
        </xsl:template>
      </xsl:stylesheet>

    val data =
      <foo foo1="1" foo2="2">
        <bar bar1="1" bar2="2"/>
      </foo>

    assertTransformMatches(xslt, data)
  }

  test("Descendant selector (//) and parent axis") {
    val xslt =
      <xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
        <xsl:template match="/">
          <result><xsl:apply-templates select="//a"/></result>
        </xsl:template>
        <xsl:template match="//a">
          <a>
            <xsl:attribute name="parent">
              <xsl:value-of select="name(..)"/>
            </xsl:attribute>
          </a>
        </xsl:template>
      </xsl:stylesheet>

    val data =
      <a>
        <a/>
        <a/>
        <b>
          <a/>
        </b>
        <a>
          <a/>
          <c>
            <a/>
          </c>
        </a>
      </a>

    assertTransformMatches(xslt, data)
  }

  test("Attribute value templates") {
    val xslt =
      <xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
        <xsl:template match="/root">
          <result attr="{@attr}">
            <xsl:apply-templates/>
          </result>
        </xsl:template>
        <xsl:template match="*">
          <xsl:element name="{name(.)}">
            <xsl:variable name="attr" select="'attr'"/>
            <xsl:attribute name="{name(.)}-{$attr}"><xsl:value-of select="name(.)"/></xsl:attribute>
          </xsl:element>
        </xsl:template>
      </xsl:stylesheet>

    val data =
      <root attr="foobar">
        <a/>
        <b/>
        <c/>
      </root>

    assertTransformMatches(xslt, data)
  }

  test("<xsl:for-each>") {
    val xslt =
      <xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
        <xsl:template match="/root">
          <result>
            <xsl:for-each select="a">
              <elem attr="{@attr}" position="{position()}-of-{last()}"/>
            </xsl:for-each>
          </result>
        </xsl:template>
      </xsl:stylesheet>

    val data =
      <root>
        <a attr="a1"/>
        <b/>
        <a attr="a2"/>
        <a attr="a3"/>
        <b/>
      </root>

    assertTransformMatches(xslt, data)
  }

  test("<xsl:copy>") {
    val xslt =
      <xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
        <xsl:template match="/root">
          <result>
            <xsl:for-each select="* | @*">
              <xsl:copy>
                <xsl:attribute name="bar">bar</xsl:attribute>
                <b/>
              </xsl:copy>
            </xsl:for-each>
          </result>
        </xsl:template>
      </xsl:stylesheet>

    val data =
      <root attr="x">
        <a foo="foo">
          <a/>
        </a>
        Some text
        <a/>
      </root>

    assertTransformMatches(xslt, data)
  }

  test("Template modes") {
    val xslt =
      <xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
        <xsl:template match="/">
          <result>
            <xsl:apply-templates/>
            <xsl:apply-templates mode="foobar" />
          </result>
        </xsl:template>

        <!-- Make sure that the built-in recursive rules propagate the mode -->
        <xsl:template match="a" mode="foobar">
          <foobar-mode>
            <!-- The following should trigger the template below -->
            <xsl:apply-templates select="."/>
          </foobar-mode>
        </xsl:template>

        <xsl:template match="a">
          <no-mode/>
        </xsl:template>
      </xsl:stylesheet>

    val data =
      <root>
        <a/>
        <a/>
      </root>

    assertTransformMatches(xslt, data)
  }

  test("String value of node-sets") {
    val xslt =
      <xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
        <xsl:template match="/">
          <result>
            <first>
              <!-- this will copy only the content of the first `p` element -->
              <xsl:value-of select="string(root/p)"/>
            </first>
            <second>
              <!-- this will copy all of the content of the `root` element -->
              <xsl:value-of select="string(root)"/>
            </second>
          </result>
        </xsl:template>
      </xsl:stylesheet>

    val data =
      <root>
        <p>This is the first paragraph.</p>
        <p foo="bar">This is the <i>second</i> paragraph.</p>
        <p>And the last one.</p>
      </root>

    assertTransformMatches(xslt, data)
  }

  test("Result Tree Fragments #1") {
    val xslt =
      <xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
        <xsl:template match="/">
          <xsl:variable name="foo">
            <result-tree-fragment node="1">
              <child/>
              <child/>
            </result-tree-fragment>
            <result-tree-fragment node="2">
              Some text
            </result-tree-fragment>
          </xsl:variable>

          <result>
            <xsl:copy-of select="$foo"/>
          </result>
        </xsl:template>
      </xsl:stylesheet>

    val data = <root/>

    assertTransformMatches(xslt, data)
  }

  test("Result Tree Fragments #2") {
    val xslt =
      <xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
        <xsl:template match="/">
          <xsl:variable name="foo">1</xsl:variable>
          <xsl:variable name="bar">
            <xsl:apply-templates select="root"/>
          </xsl:variable>
          <result>
            <xsl:copy-of select="number($foo)"/>
            <xsl:copy-of select="$bar"/>
          </result>
        </xsl:template>

        <xsl:template match="a|b|c">
          <xsl:variable name="name"><xsl:copy-of select="name(.)"/></xsl:variable>
          <xsl:element name="{$name}"/>
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

  test("Multiple result elements (invalid)") {
    val xslt =
      <xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
        <xsl:template match="/">
          <a/>
          <b/>
        </xsl:template>
      </xsl:stylesheet>

    val data = <root/>

    assertTransformMatches(xslt, data)
  }

  test("Global variables/parameters") {
    val xslt =
      <xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
        <xsl:param name="a" select="1"/>
        <xsl:variable name="b" select="$a + 1"/>
        <xsl:variable name="x" select="5"/>
        <xsl:template match="/">
          <result>
            <xsl:call-template name="foobar">
              <xsl:with-param name="a" select="$b"/>
            </xsl:call-template>
          </result>
        </xsl:template>
        <xsl:template name="foobar">
          <xsl:param name="a" select="0"/> <!-- this should shadow the global param `a` -->
          <xsl:param name="c" select="$a + 1"/> <!-- this should be evaluated after `a` -->
          <xsl:value-of select="$c + $x"/> <!-- should be 3 + 5 = 8 -->
        </xsl:template>
      </xsl:stylesheet>

    val data = <root/>

    assertTransformMatches(xslt, data)
  }

  def checkMatch(transformed: Either[T, Exception], referenceResult: Either[Elem, Exception]) = {
    // if Java throws an exception, we should do the same (because of invalid input)
    (referenceResult, transformed) match {
      case (Left(ref), Left(trans)) => assertResult(XMLParser.parseDocument(ref)) { trans }
      case (Right(eScala), Right(eJava)) =>
        println(f"Scala error: $eScala")
        println(f"Java error: $eJava")
      case _ => assertResult(referenceResult) { transformed } // one side threw an error while the other didn't
    }
  }

  def transform(xslt: Elem, data: Elem): T

  def assertTransformMatches(xslt: Elem, data: Elem) = {
    val referenceResult = try {
      Left(TransformHelper.transformJava(xslt, data))
    } catch {
      case eJava: TransformerException => Right(eJava)
      case eJava: RuntimeException => Right(eJava)
      case eJava: SAXParseException => Right(eJava) // this is thrown when the resulting document is invalid
    }
    println(referenceResult)

    val testResult = try {
      Left(transform(xslt, data))
    } catch {
      case eScala: ProcessingError => Right(eScala)
      case eScala: RuntimeException => Right(eScala)
    }

    println(f"Result: $testResult")

    checkMatch(testResult, referenceResult)
  }
}
