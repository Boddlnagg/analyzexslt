<?xml version="1.0" ?>
<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
    xmlns:svg="http://www.w3.org/2000/svg" xmlns:xlink="http://xlink.org">

    <xsl:import href="dinosaurs.xsl" />
    <xsl:import href="graphtools.xsl" />
    <xsl:output method="html"/>

    <xsl:param name="maxX" select="count(/dinosaurs/dinosaur)*100" />
    <xsl:param name="yOffset" select="120" />
    <xsl:param name="xOffset" select="100" />

    <xsl:template match="dinosaurs" mode="svgforweights">
	<xsl:param name="maxY">400</xsl:param>
	<h2>Dinosaur weights</h2>

	<!-- The +1 for maxX allows the last line to be displayed -->
	<svg:svg width="400px" height="400px" viewBox="0 0 {$maxX + $xOffset} {$maxY}" preserveAspectRatio="none">
	    <!-- Stylesheets, and styles in general, don't seem to work with inline SVG -->
	    <xsl:call-template name="graphStyles" />
	    <svg:defs>
		<xsl:call-template name="graphFilters" />
	    </svg:defs>
	    <!-- Draw grid lines -->
	    <xsl:call-template name="drawLines" >
		<xsl:with-param name="yAxisTitle" select="'Weight / tons'" />
		<xsl:with-param name="yOffset" select="$yOffset" />
		<xsl:with-param name="maxX" select="$maxX" />
		<xsl:with-param name="maxY" select="$maxY" />
	    </xsl:call-template>

	    <xsl:for-each select="dinosaur">
		    <xsl:call-template name="drawBar">
			<xsl:with-param name="height_condition"><xsl:value-of select="number(weight/text())"/></xsl:with-param>
			<xsl:with-param name="maxY" select="$maxY" />
			<xsl:with-param name="yOffset" select="$yOffset" />
		    </xsl:call-template>
	    </xsl:for-each>
	</svg:svg>
    </xsl:template>

   <xsl:template match="dinosaurs" mode="svgforheights">
	<xsl:param name="maxY">300</xsl:param>
	<h2>Dinosaur heights</h2>
	<!-- The +1 for maxX allows the last line to be displayed -->
	<svg:svg width="400px" height="400px" viewBox="0 0 {$maxX + $xOffset} {$maxY}" preserveAspectRatio="none">
	    <!-- Stylesheets, and styles in general, don't seem to work with inline SVG -->
	    <xsl:call-template name="graphStyles" />
	    <svg:defs>
		<xsl:call-template name="graphFilters" />
	    </svg:defs>
	    <!-- Draw grid lines -->
	    <xsl:call-template name="drawLines" >
		<xsl:with-param name="yAxisTitle" select="'Length / m'" />
		<xsl:with-param name="yOffset" select="$yOffset" />
		<xsl:with-param name="maxX" select="$maxX" />
		<xsl:with-param name="maxY" select="$maxY" />
	    </xsl:call-template>

	    <xsl:for-each select="dinosaur">
		<xsl:call-template name="drawBar">
		    <xsl:with-param name="height_condition"><xsl:value-of select="number(length/text())"/></xsl:with-param>
		    <xsl:with-param name="maxY" select="$maxY" />
		    <xsl:with-param name="yOffset" select="$yOffset" />
		</xsl:call-template>
	    </xsl:for-each>
	</svg:svg>
    </xsl:template>

    <xsl:template match="dinosaurs">
	<html xmlns:svg="http://www.w3.org/2000/svg">

	<xsl:call-template name="svgDisplayerInclude" />

	<head>
	    <title>A simple HTML page</title>
	    <style type="text/css">
		body { font-family: Verdana, Times, Serif; }
	    </style>
	</head>
	<body>
	    <h1>Dinosaur graphs</h1>
	<!-- SVG for showing the graph of weights -->
	    <xsl:apply-templates select="." mode="svgforweights"/>
	<!-- SVG for heights graph -->
	    <xsl:apply-templates select="." mode="svgforheights"/>
	<!-- Now return to the normal body of the text -->
	    <h1>Dinosaur details</h1>
	    
	    <xsl:apply-templates />
	</body>
	</html>
    </xsl:template>

</xsl:stylesheet>
