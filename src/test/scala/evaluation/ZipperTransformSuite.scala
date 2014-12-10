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

    // The following expected result corresponds to this pseudo-XML-document:
    // <result>???</result>

    assertResult(
      Subtree(Set(Root),ZNil(),ZCons(
        Subtree(Set(Element("result")),ZNil(),ZMaybeNil(
          Subtree(Set(AnyText),ZNil(),ZNil()), ZNil()
        )), ZNil()
      )
    )) { transform(xslt)._1 }
  }

  test("Multiple attributes") {
    // the output of this stylesheet is completely independent of the input
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

    assertResult(parser.parseDocument(<result literal1="foo" literal2="bar" dynamic1="1" dynamic2="2" dynamic3="3"/>)) {
      transform(xslt)
    }
  }

  test("Node names") {
    val xslt =
      <xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
        <xsl:template match="/"><xsl:apply-templates/></xsl:template>
        <xsl:template match="/*">
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

    // The following expected result corresponds to this pseudo-XML-document:
    // <result>
    //  <attribute|element name="???"/>
    //  ... (more of the same form as above)
    // </result>

    assertResult(
      Subtree(Set(Root),ZNil(),ZCons(
        Subtree(Set(Element("result")),
          ZNil(), // attributes of <result>
          ZUnknownLength( // children of <result>
              Subtree(Set(Element("attribute"), Element("element")),ZUnknownLength(Set(NamedAttribute("name"))),ZNil())
          )
        ), ZNil()
      ))) { transform(xslt)._1 }
  }

  // TODO: ignored because of non-termination
  ignore("Replicate n times") {
    val xslt =
      <xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
        <xsl:template match="/"><xsl:apply-templates/></xsl:template>
        <xsl:template match="/*">
          <result>
          <xsl:apply-templates select="*">
            <xsl:with-param name="n" select="number(@n)"/>
          </xsl:apply-templates>
        </result>
        </xsl:template>
        <xsl:template match="*">
          <xsl:param name="n" select="0"/>
          <xsl:call-template name="do-copy">
            <xsl:with-param name="n" select="$n"/>
          </xsl:call-template>
        </xsl:template>
        <xsl:template name="do-copy">
          <xsl:param name="n" select="0"/>
          <xsl:if test="$n > 0">
            <xsl:copy-of select="."/>
            <xsl:call-template name="do-copy">
              <xsl:with-param name="n" select="$n - 1"/>
            </xsl:call-template>
          </xsl:if>
        </xsl:template>
      </xsl:stylesheet>

    assertResult(???) { transform(xslt)._1 }
  }

  test("Wikipedia (Java Example)") {
    // taken from http://en.wikipedia.org/wiki/Java_API_for_XML_Processing#Example
    val xslt =
      <xsl:stylesheet version='1.0' xmlns:xsl='http://www.w3.org/1999/XSL/Transform'>
        <xsl:template match='/'>
          <reRoot><reNode><xsl:value-of select='/root/node/@val' /> world</reNode></reRoot>
        </xsl:template>
      </xsl:stylesheet>

    // The following expected result corresponds to this pseudo-XML-document:
    // <reRoot>
    //   <reNode>???</reNode>
    // </reRoot>

    assertResult(
      Subtree(Set(Root),ZNil(),ZCons(
        Subtree(Set(Element("reRoot")),ZNil(),ZCons(
          Subtree(Set(Element("reNode")),ZNil(),ZCons(
            Subtree(Set(AnyText),ZNil(),ZNil()),ZNil()
          )), ZNil()
        )), ZNil()))
    ) { transform(xslt)._1 }
  }

  test("Wikipedia (XSLT #1 simplified)") {
    val xslt =
      <xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" version="1.0">
        <xsl:template match="/"><xsl:apply-templates select="persons"/></xsl:template>
        <xsl:template match="/persons">
          <root>
            <xsl:apply-templates select="person"/>
          </root>
        </xsl:template>
        <xsl:template match="person">
          <name>
            <xsl:attribute name="username"><xsl:value-of select="@username"/></xsl:attribute>
            <xsl:value-of select="name" />
          </name>
        </xsl:template>
      </xsl:stylesheet>

    // The following expected result corresponds to this pseudo-XML-document:
    // <root>
    //   <name username="???">???</name>
    //   <name username="???">???</name>
    //   ... (more <name>s)
    // </root>

    assertResult(
      Subtree(Set(Root),ZNil(),ZCons(
        Subtree(Set(Element("root")),ZNil(),ZUnknownLength(
          Subtree(Set(Element("name")),ZUnknownLength(Set(NamedAttribute("username"))),ZMaybeNil(
            Subtree(Set(AnyText),ZNil(),ZNil()), ZNil()
          ))
        )), ZNil()
      ))) { transform(xslt)._1 }
  }

  test("Wikipedia (XSLT #2 simplified)") {
    assertResult(
      Subtree(Set(Root),ZNil(),ZList(
        Subtree(Set(Element("li"), Element("html")),ZNil(),ZCons(
          Subtree(Set(AnyText, Element("head")),ZNil(),ZMaybeNil(
            Subtree(Set(Element("title")),ZNil(),ZList(
              Subtree(Set(Text("Testing XML Example")),ZNil(),ZNil())
            )), ZNil()
          )),
          ZMaybeNil(Subtree(Set(Element("body")),ZNil(),ZList(
            Subtree(Set(Element("h1")),ZNil(),ZList(Subtree(Set(Text("Persons")),ZNil(),ZNil()))),
            Subtree(Set(Element("ul")),ZNil(),ZUnknownLength(
              Subtree(Set(Element("li")),ZNil(),ZList(
                Subtree(Set(AnyText),ZNil(),ZNil())
              ))
            ))
          )),ZNil())
        ))
      ))
    ) { transform(TestData.WikipediaStylesheet2)._1 }
  }

  test("XSLT book example (recursive descend)") {
    val xslt =
      <xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" version="1.0">
        <xsl:template match="/"><xsl:apply-templates select="books"/></xsl:template>
        <xsl:template match="books">
          <html>
            <body>
              <h1>A list of books</h1>
              <table>
                <tr>
                  <th>Title</th>
                  <th>Author</th>
                  <th>Price</th>
                </tr>
                <xsl:apply-templates select="book"/>
              </table>
            </body>
          </html>
        </xsl:template>
        <xsl:template match="book">
          <tr>
            <xsl:apply-templates select="title"/>
            <xsl:apply-templates select="author"/>
            <xsl:apply-templates select="price"/>
          </tr>
        </xsl:template>
        <xsl:template match="author | title | price">
          <td><xsl:value-of select="."/></td>
        </xsl:template>
      </xsl:stylesheet>

    // The following expected result corresponds to this pseudo-XML-document:
    // <html>
    //   <body>
    //     <h1>A list of books</h1>
    //     <table>
    //       <tr>
    //         <th>Title</th>
    //         <th>Author</th>
    //         <th>Price</th>
    //       </tr>
    //       <tr>
    //         <td>???</td>
    //         ... (more <td>s -> there could be multiple <title> elements, ...)
    //       </tr>
    //       ... (more <tr>s of the same form as before)
    //     </table>
    //   </body>
    // </html>

    assertResult(
      Subtree(Set(Root),ZNil(),ZList(
        Subtree(Set(Element("html")),ZNil(),ZList(
          Subtree(Set(Element("body")),ZNil(),ZList(
            Subtree(Set(Element("h1")),ZNil(),ZList(
              Subtree(Set(Text("A list of books")),ZNil(),ZNil())
            )),
            Subtree(Set(Element("table")),ZNil(),ZCons(
              Subtree(Set(Element("tr")),ZNil(),ZList(
                Subtree(Set(Element("th")),ZNil(),ZList(Subtree(Set(Text("Title")),ZNil(),ZNil()))),
                Subtree(Set(Element("th")),ZNil(),ZList(Subtree(Set(Text("Author")),ZNil(),ZNil()))),
                Subtree(Set(Element("th")),ZNil(),ZList(Subtree(Set(Text("Price")),ZNil(),ZNil())))
              )),
              ZUnknownLength(Subtree(Set(Element("tr")),ZNil(),ZUnknownLength(
                Subtree(Set(Element("td")),ZNil(),ZMaybeNil(Subtree(Set(AnyText),ZNil(),ZNil()),ZNil()))
              )))
            ))
          ))
        ))
      ))
    ) { transform(xslt)._1 }
  }

  test("Descendant selector (//) and parent axis") {
    // TODO: could use //b/a as selector for the second template, but currently this does not terminate
    val xslt =
      <xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
        <xsl:template match="/">
          <result><xsl:apply-templates select="//a"/></result>
        </xsl:template>
        <xsl:template match="b/a">
          <a>
            <xsl:attribute name="parent">
              <xsl:value-of select="name(..)"/>
            </xsl:attribute>
          </a>
        </xsl:template>
      </xsl:stylesheet>

    // The following expected result corresponds to this pseudo-XML-document:
    // <result>
    // <a parent="b"/>
    // ... (more <a>s)
    // </result>

    assertResult(
      Subtree(Set(Root),ZNil(),ZList(
        Subtree(Set(Element("result")),ZNil(),ZUnknownLength(
          Subtree(Set(Element("a")),ZUnknownLength(Set(Attribute("parent", "b"))),ZNil())
        ))
      ))
    ) { transform(xslt)._1 }
  }
}
