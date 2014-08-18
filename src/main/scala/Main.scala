object Main {

  def main(args: Array[String]) {
    val sheet = new XSLTStylesheet(stylesheet);
    println(sheet.templates.toString)
  }

  val stylesheet =
  <xsl:stylesheet version="1.0"
                  xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
    <xsl:template match="/">
      <html>
        <head>
          <title>Hello XHTML world</title>
        </head>
        <body>
          <xsl:if test="true">
            <h1>Hello world</h1>
          </xsl:if>
          <p><a href="scala-lang.org">Scala</a> talks XHTML</p>
        </body>
      </html>
    </xsl:template>
  </xsl:stylesheet>;
}