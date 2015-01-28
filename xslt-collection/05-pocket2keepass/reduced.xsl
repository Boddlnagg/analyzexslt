<?xml version="1.0" encoding="utf-8"?>

<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
     version="1.0">
<xsl:output method="xml" encoding="utf-8"/>

<xsl:template match="@*|text()"/>

  <xsl:template match="groups">
    <xsl:element name="database" >
      <xsl:apply-templates select="*|@*|text()"/>
    </xsl:element>
  </xsl:template>

  <xsl:template match="group">
    <xsl:element name="group" >
      <xsl:element name="title">
         <xsl:value-of select="@title"/>
      </xsl:element>
      <xsl:apply-templates select="*|@*|text()"/>
    </xsl:element>
  </xsl:template>

  <xsl:template match="entry">
    <xsl:element name="entry" >
      <xsl:element name="title">
         <xsl:value-of select="@title"/>
      </xsl:element>
      <xsl:apply-templates select="*|@*|text()"/>
    </xsl:element>
  </xsl:template>

  <xsl:template match="field">
    <xsl:element name="{translate(@title,'./ ','')}">
      <xsl:value-of select="@value"/>
    </xsl:element>
  </xsl:template>

</xsl:stylesheet>
