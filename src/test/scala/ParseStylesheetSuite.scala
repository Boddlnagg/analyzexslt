import org.scalatest.FunSuite

class ParseStylesheetSuite extends FunSuite {
  // This data and the two stylesheets are taken from https://en.wikipedia.org/wiki/XSLT
  val data =
    <persons>
      <person username="JS1">
        <name>John</name>
        <family-name>Smith</family-name>
      </person>
      <person username="MI1">
        <name>Morka</name>
        <family-name>Ismincius</family-name>
      </person>
    </persons>

  test("Parse stylesheet (Wikipedia #1 simplified)") {
    val stylesheet =
      <xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" version="1.0">

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
    val parsed = new XSLTStylesheet(stylesheet)
    assert(parsed.namedTemplates.isEmpty, "There must not be any named templates")
    assert(parsed.matchableTemplates
      .filter {case (_, _, _, importPrecedence) => importPrecedence == XSLT.UserDefinedImportPrecedence}
      .size == 2, "There must be 2 user defined matchable templates")
  }

  test("Parse stylesheet (Wikipedia #2 simplified)") {
    val stylesheet =
      <xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" version="1.0">
        <xsl:template match="/persons">
          <html>
            <head>
              <title>Testing XML Example</title>
            </head>
            <body>
              <h1>Persons</h1>
              <ul>
                <xsl:apply-templates select="person">
                  <!--<xsl:sort select="family-name"/>-->
                </xsl:apply-templates>
              </ul>
            </body>
          </html>
        </xsl:template>

        <xsl:template match="person">
          <li>
            <xsl:value-of select="family-name"/> <xsl:text>, </xsl:text> <xsl:value-of select="name"/>
          </li>
        </xsl:template>

      </xsl:stylesheet>
    val parsed = new XSLTStylesheet(stylesheet)
    assert(parsed.namedTemplates.isEmpty, "There must not be any named templates")
    assert(parsed.matchableTemplates
                 .filter {case (_, _, _, importPrecedence) => importPrecedence == XSLT.UserDefinedImportPrecedence}
                 .size == 2, "There must be 2 user defined matchable templates")
  }
}
