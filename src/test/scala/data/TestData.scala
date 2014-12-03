package data

object TestData {
  // taken from https://en.wikipedia.org/wiki/XSLT
  val WikipediaData =
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

  val WikipediaStylesheet1 =
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

  val WikipediaStylesheet2 =
    <xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" version="1.0">
      <!--<xsl:template match="text()"></xsl:template>-->
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

  // taken from http://www.w3schools.com/dom/dom_nodes.asp
  val W3SchoolsData =
    <bookstore>
      <book category="cooking">
        <title lang="en">Everyday Italian</title>
        <author>Giada De Laurentiis</author>
        <year>2005</year>
        <price>30.00</price>
      </book>
      <book category="children">
        <title lang="en">Harry Potter</title>
        <author>J K. Rowling</author>
        <year>2005</year>
        <price>29.99</price>
      </book>
      <book category="web">
        <title lang="en">XQuery Kick Start</title>
        <author>James McGovern</author>
        <author>Per Bothner</author>
        <author>Kurt Cagle</author>
        <author>James Linn</author>
        <author>Vaidyanathan Nagarajan</author>
        <year>2003</year>
        <price>49.99</price>
      </book>
      <book category="web" cover="paperback">
        <title lang="en">Learning XML</title>
        <author>Erik T. Ray</author>
        <year>2003</year>
        <price>39.95</price>
      </book>
    </bookstore>

  // taken from http://nwalsh.com/docs/tutorials/xsl/xsl/foil32.html and slightly modified
  val NamedTemplateExampleStylesheet =
    <xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" version="1.0">

      <xsl:template match="/">
        <!-- this root element/template was not there in the original source -->
        <html>
          <xsl:apply-templates/>
        </html>
      </xsl:template>

      <xsl:template name="admonition">
        <xsl:param name="type" select="'Warning'"/>
        <table border="1">
          <tr><th><xsl:value-of select="$type"/>:</th></tr>
          <tr><td><xsl:apply-templates/></td></tr>
        </table>
      </xsl:template>

      <xsl:template match="warning">
        <xsl:call-template name="admonition"/>
      </xsl:template>

      <xsl:template match="caution">
        <xsl:call-template name="admonition">
          <xsl:with-param name="type" select="'Caution'"/>
        </xsl:call-template>
      </xsl:template>

      <xsl:template match="para">
        <p><xsl:apply-templates/></p>
      </xsl:template>

      <xsl:template match="emphasis">
        <i><xsl:apply-templates/></i>
      </xsl:template>
    </xsl:stylesheet>

  val NamedTemplateExampleData =
    <chapter>
      <warning>
        <para>Using a damaged extension cord may cause a fire.</para>
      </warning>
      <caution>
        <para>Freshly brewed coffee is hot.</para>
      </caution>
    </chapter>

}
