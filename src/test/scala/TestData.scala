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
}
