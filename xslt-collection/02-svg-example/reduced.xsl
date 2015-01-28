<?xml version="1.0" ?>
<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
    xmlns:svg="http://www.w3.org/2000/svg" xmlns:xlink="http://xlink.org">

    <!--<xsl:import href="dinosaurs.xsl" />-->
    <!--
	Required for IE 5 and 5.5 - see the first stylesheet in this chapter.
    -->
    <xsl:template match="/">
	<xsl:apply-templates />
    </xsl:template>

    <!-- 
	There is a built-in rule for text that it is simply passed
	through and displayed.

	In this case we don't want to do that - instead we want to
	suppress it by default, and only display it when we 
	specifically match it with value-of rules.
    -->
    <xsl:template match="text()" /> 

    <!--
	Match the "dinosaurs" element that wraps all of the
	"dinosaur" elements, add a header and footer, and apply
	templates to all the "dinosaur" elements inside it.

	Note that this is the same header and footer that we've
	used in the previous examples.

	In practice, if we were using the same header and footer
	text in several stylesheets, we might well move it to a
	parent stylesheet and use xsl:import or xsl:include to
	include it in each. (Although neither of these
	work in IE 5 or IE 5.5).
    -->
    <xsl:template match="dinosaurs">
	<!-- Standard header - this is just a chunk of plain HTML -->
	<html>
	<head>
	    <title>A simple HTML page</title>
	    <style type="text/css">
		body { font-family: Verdana, Times, Serif; }
	    </style>
	</head>
	<body>

	    <table style="border: solid thin black">
		<tr>
		    <td><a href="mammoth.html">Visit the Mammoth zone!</a> - </td>
		    <td><a href="play.html">Play Pteranodon Poker</a></td>
		</tr>
	    </table>

	    <h1>My big list of dinosaurs</h1>

	<!-- Apply templates to everything inside the "dinosaurs" element -->
	    <xsl:apply-templates />

	<!-- Standard footer -->
	<hr/>
	Copyright 2002 DinosaurOrg.
	</body>
	</html>
    </xsl:template>

    <!-- 
	Match the "dinosaur" element, and display its name inside
	an h2 tag
    -->
    <xsl:template match="dinosaur">
	<!-- 
	    The XPath syntax for matching an attribute is
	    just "@attribute" - so this selects the text
	    in the name attribute of dinosaur:
	    <dinosaur name="THIS_IS_MATCHED">..</dinosaur>
	-->
	<h2><xsl:value-of select="@name"/></h2>

	<!--
	    Because at the top of the stylesheet we've disabled 
	    the default match for text(), we need to use value-of
	    to pull it directly out of the XML.
	-->
	<xsl:value-of select="description/text()"/>
	<ul>
	    <!-- 
		And now we apply templates to everything inside the
		dinosaur tag - this is "description", "weight", 
		"length" and "color".

		We do not have a template matching "description" in
		this stylesheet, so it will be ignored. The others
		have templates defined for them below. 
	    -->
	    <xsl:apply-templates/>
	</ul>
    </xsl:template>

    <!--
	Match the "weight" element that appears inside "dinosaur",
	and display its value.

	Notice that the space after "Weight: " is inside the <b>
	tags, rather than outside it. The rules on whitespace, as
	described in chapter 6, mean that this is counted as significant
	whitespace. Whitespace that has text before or after it is significant,
	whitespace that has a tag on either side of it is not significant,
	and is ignored.

	We could force the whitespace to be included if it didn't have
	text on one side of it by putting it inside an xslt:text tag - 
	for example <xslt:text> </xslt:text> would be another way
	of including a space.

	This match for "weight" is almost exactly the same as that
	for "length" and "color". In this case it isn't worth it,
	but if the shared text was larger, then we might think about
	using a named template containing the shared text, and calling
	it from each of them. Named templates are described in chapter 6.
    -->
    <xsl:template match="weight">
	<li><b>Weight: </b><xsl:value-of select="text()" /> tons</li>
    </xsl:template>

    <!-- Almost exactly the same as "weight" -->
    <xsl:template match="length">
	<li><b>Length: </b><xsl:value-of select="text()" /> m</li>
    </xsl:template>

    <!-- Again, almost exactly the same as "weight" -->
    <xsl:template match="color">
	<li><b>Color: </b><xsl:value-of select="text()" /></li>
    </xsl:template>
    
    <!--<xsl:import href="graphtools.xsl" />-->
    <xsl:template name="svgDisplayerInclude">
	<object id="AdobeSVG" CLASSID="clsid:78156a80-c6a1-4bbf-8e6a-3cd390eeb4e2"></object>
	<!--<xsl:processing-instruction name = "import" > namespace="svg" implementation="#AdobeSVG"</xsl:processing-instruction> -->
    </xsl:template>

    <xsl:template name="graphStyles">
	<!-- Note - styles don't appear to work with the Adobe SVG plugin
	    when the SVG is included inline as part of an XHTML document
	-->
	<svg-style type="text/css"><![CDATA[
	    text { fill:black; font-size:12; font-family:Arial }
	    .gridline { stroke: red; stroke-opacity: 0.2; }
	    .axis { stroke: blue; stroke-opacity: 1.0; }
	    .bar { stroke:rgb(128,128,255); stroke-width:1; filter:url(#Drop_Shadow) }
	    .barLabel { writing-mode: tb; font-size: 12; }
	    .axisLabel { text-anchor: end; }
	    .title { text-anchor: middle; font-size: 24; }
	]]></svg-style>
    </xsl:template>

    <xsl:template name="graphFilters">
	<svg-filter id="Drop_Shadow" filterUnits="objectBoundingBox" x="-10%" y="-10%"
		width="150%" height="150%">
		<svg-feGaussianBlur in="SourceAlpha" stdDeviation="3" result="blurredAlpha"/>
		<svg-feOffset in="blurredAlpha" dx="3" dy="3" result="offsetBlurredAlpha"/>
		<svg-feFlood result="flooded" style="flood-color:rgb(0,0,0);flood-opacity:0.65"/>
		<svg-feComposite in="flooded" operator="in" in2="offsetBlurredAlpha"
			result="coloredShadow"/>
		<svg-feComposite in="SourceGraphic" in2="coloredShadow" operator="over"/>
	</svg-filter>
    </xsl:template>

    <xsl:template name="drawBar" >
	    <xsl:param name="height_condition" />
	    <xsl:param name="bar_width" >60</xsl:param>
	    <xsl:param name="bar_separation">20</xsl:param>
	    <xsl:param name="maxY" />
	    <xsl:param name="yOffset" />
    
	    <svg-rect x="{ (position() * ($bar_width + ($bar_separation * 2))) + ($bar_separation) }" y="{$maxY - $yOffset - $height_condition}" 
		      width="{$bar_width}" height="{$height_condition}" 

		      class="bar" fill="rgb(128,128,255)" filter="url(#Drop_Shadow)" />
<!--
	    Script doesn't work when SVG is included using the
	    svg namespace within an XHTML document with the Adobe plugin.
		onmouseover="evt.getTarget().setAttribute('width', 50)"
		onmouseout="evt.getTarget().setAttribute('width', 26)"
-->

	    <svg-text x="{ ($bar_width div 2) +(position() * ($bar_width + ($bar_separation * 2))) + ($bar_separation) }" 
		    y="{$maxY - $yOffset + 5}" writing-mode="tb" 
		    class="barLabel">
		<xsl:value-of select="@name"/>
	    </svg-text>
    </xsl:template>



    <xsl:template name="drawLines">
	<xsl:param name="maxY">500</xsl:param>
	<xsl:param name="xOffset">100</xsl:param>
	<xsl:param name="yOffset">100</xsl:param>
	<xsl:param name="maxX" />
	<xsl:param name="xAxisGridInterval">20</xsl:param>
	<xsl:param name="yAxisGridInterval">20</xsl:param>
	<xsl:param name="labelInterval">40</xsl:param>
	<xsl:param name="drawXLabels">false</xsl:param>
	<xsl:param name="drawYLabels">true</xsl:param>
	<xsl:param name="title" />
	<xsl:param name="yAxisTitle" />
	<xsl:param name="xAxisTitle" />

	<xsl:call-template name="drawgridVertical" >
	    <xsl:with-param name="maxX" select="$maxX" />
	    <xsl:with-param name="maxY" select="$maxY" />
	    <xsl:with-param name="interval" select="$xAxisGridInterval" />
	    <xsl:with-param name="xOffset" select="$xOffset" />
	    <xsl:with-param name="yOffset" select="$yOffset" />
	</xsl:call-template>
	<xsl:call-template name="drawgridHorizontal">
	    <xsl:with-param name="maxX" select="$maxX" />
	    <xsl:with-param name="maxY" select="$maxY" />
	    <xsl:with-param name="interval" select="$yAxisGridInterval" />
	    <xsl:with-param name="xOffset" select="$xOffset" />
	    <xsl:with-param name="yOffset" select="$yOffset" />
	</xsl:call-template>
	<xsl:call-template name="xAxis">
	    <xsl:with-param name="maxX" select="$maxX" />
	    <xsl:with-param name="maxY" select="$maxY" />
	    <xsl:with-param name="interval" select="$labelInterval" />
	    <xsl:with-param name="xOffset" select="$xOffset" />
	    <xsl:with-param name="yOffset" select="$yOffset" />
	    <xsl:with-param name="drawXLabels" select="$drawXLabels" />
	    <xsl:with-param name="xAxisTitle" select="$xAxisTitle" />
	</xsl:call-template>
	<xsl:call-template name="yAxis">
	    <xsl:with-param name="maxX" select="$maxX" />
	    <xsl:with-param name="maxY" select="$maxY" />
	    <xsl:with-param name="interval" select="$labelInterval" />
	    <xsl:with-param name="xOffset" select="$xOffset" />
	    <xsl:with-param name="yOffset" select="$yOffset" />
	    <xsl:with-param name="drawYLabels" select="$drawYLabels" />
	    <xsl:with-param name="yAxisTitle" select="$yAxisTitle" />
	</xsl:call-template>
	<xsl:call-template name="title">
	    <xsl:with-param name="maxX" select="$maxX" />
	    <xsl:with-param name="maxY" select="$maxY" />
	    <xsl:with-param name="interval" select="$labelInterval" />
	    <xsl:with-param name="xOffset" select="$xOffset" />
	    <xsl:with-param name="yOffset" select="$yOffset" />
	    <xsl:with-param name="title" select="$title" />
	</xsl:call-template>
    </xsl:template>

    <xsl:template name="xAxis">
	<xsl:param name="maxY" />
	<xsl:param name="maxX" />
	<xsl:param name="interval" />
	<xsl:param name="xOffset" />
	<xsl:param name="yOffset" />
	<xsl:param name="drawXLabels" />
	<xsl:param name="xAxisTitle" />
	
	<svg-line class="axis" stroke="blue" x1="{$xOffset}" y1="{$maxY - $yOffset}" 
		    x2="{$maxX + $xOffset}" y2="{$maxY - $yOffset}" />

	<svg-text x="{$xOffset + ($maxX div 2)}"
		y="{$maxY - ($yOffset div 2)}"
		class="axisLabel">
	    <xsl:value-of select="$xAxisTitle"/>
	</svg-text>

	<xsl:if test="$drawXLabels = 'true'">
	    <xsl:call-template name="xAxisLabels">
		<xsl:with-param name="maxX" select="$maxX" />
		<xsl:with-param name="maxY" select="$maxY" />
		    <xsl:with-param name="xOffset" select="$xOffset" />
		    <xsl:with-param name="yOffset" select="$yOffset" />
		<xsl:with-param name="interval" select="$interval" />
	    </xsl:call-template>
	</xsl:if>
    </xsl:template>
    
    <xsl:template name="yAxis">
	<xsl:param name="maxY" />
	<xsl:param name="maxX" />
	<xsl:param name="interval" />
	<xsl:param name="xOffset" />
	<xsl:param name="yOffset" />
	<xsl:param name="drawYLabels" />
	<xsl:param name="yAxisTitle" />

	<svg-line class="axis" stroke="blue" x1="{$xOffset}" y1="0" 
		    x2="{$xOffset}" y2="{$maxY - $yOffset}" />

	<svg-text x="{$xOffset div 2}"
		y="{($maxY div 2) - $yOffset}" writing-mode="tb" 
		class="axisLabel">
	    <xsl:value-of select="$yAxisTitle"/>
	</svg-text>


	<xsl:if test="$drawYLabels = 'true'">
	    <xsl:call-template name="yAxisLabels">
		<xsl:with-param name="maxX" select="$maxX" />
		<xsl:with-param name="maxY" select="$maxY" />
		    <xsl:with-param name="xOffset" select="$xOffset" />
		    <xsl:with-param name="yOffset" select="$yOffset" />
		<xsl:with-param name="interval" select="$interval" />
	    </xsl:call-template>
	</xsl:if>
    </xsl:template>

    <xsl:template name="title">
	<xsl:param name="maxY" />
	<xsl:param name="maxX" />
	<xsl:param name="interval" />
	<xsl:param name="xOffset" />
	<xsl:param name="yOffset" />
	<xsl:param name="title" />

	<svg-text class="title" x="{$xOffset + ($maxX div 2)}" y="{$yOffset div 2}" text-anchor="middle">
	    <xsl:value-of select="$title"/>
	</svg-text>
    </xsl:template>

    <xsl:template name="yAxisLabels">
	<xsl:param name="maxX" />
	<xsl:param name="maxY" />
	<xsl:param name="interval" />
	<xsl:param name="xOffset" />
	<xsl:param name="yOffset" />
	<xsl:param name="currentY">0</xsl:param>

	<svg-text x="{$xOffset}"
		y="{$maxY - $currentY - $yOffset }"
		text-anchor="end"
		class="axisLabel">
	    <xsl:value-of select="$currentY"/>
	</svg-text>

	<xsl:if test="$currentY &lt; ($maxY - $yOffset)">
	    <xsl:call-template name="yAxisLabels">
		<xsl:with-param name="maxX" select="$maxX" />
		<xsl:with-param name="maxY" select="$maxY" />
		<xsl:with-param name="xOffset" select="$xOffset" />
		<xsl:with-param name="yOffset" select="$yOffset" />
		<xsl:with-param name="interval" select="$interval" />
		<xsl:with-param name="currentY" select="$currentY + $interval" />
	    </xsl:call-template>
	</xsl:if>
    </xsl:template>

    <xsl:template name="drawgridVertical">
	<xsl:param name="maxY" />
	<xsl:param name="maxX" />
	<xsl:param name="interval" />
	<xsl:param name="xOffset" />
	<xsl:param name="yOffset" />
	<xsl:param name="currentX" select="$xOffset" />

	<svg-line class="gridline" x1="{$currentX}" y1="0" x2="{$currentX}" y2="{$maxY - $yOffset}" />	

	<xsl:if test="($currentX  )&lt; ($maxX + $xOffset )">
	    <xsl:call-template name="drawgridVertical">
		<xsl:with-param name="maxX" select="$maxX" />
		<xsl:with-param name="maxY" select="$maxY" />
		<xsl:with-param name="xOffset" select="$xOffset" />
		<xsl:with-param name="yOffset" select="$yOffset" />
		<xsl:with-param name="interval" select="$interval" />
		<xsl:with-param name="currentX" select="$currentX + $interval" />
	    </xsl:call-template>
	</xsl:if>
    </xsl:template>


    <xsl:template name="drawgridHorizontal">
	<xsl:param name="maxY" />
	<xsl:param name="maxX" />
	<xsl:param name="interval" />
	<xsl:param name="xOffset" />
	<xsl:param name="yOffset" />
	<xsl:param name="currentY" select="$yOffset + ($yOffset mod $interval)" />

	<svg-line class="gridline" x1="{$xOffset}" y1="{$currentY - $yOffset}" x2="{$maxX + $xOffset}" y2="{$currentY - $yOffset}" />	

	<xsl:if test="( $currentY + $interval ) &lt; $maxY">
	    <xsl:call-template name="drawgridHorizontal">
		<xsl:with-param name="maxX" select="$maxX" />
		<xsl:with-param name="maxY" select="$maxY" />
		<xsl:with-param name="xOffset" select="$xOffset" />
		<xsl:with-param name="yOffset" select="$yOffset" />
		<xsl:with-param name="interval" select="$interval" />
		<xsl:with-param name="currentY" select="$currentY + $interval" />
	    </xsl:call-template>
	</xsl:if>
    </xsl:template>

    <xsl:template name="xAxisLabels">
	<xsl:param name="maxX" />
	<xsl:param name="maxY" />
	<xsl:param name="interval" />
	<xsl:param name="xOffset" />
	<xsl:param name="yOffset" />
	<xsl:param name="currentX" select="$xOffset" />

	<svg-text class="axisLabel" x="{$currentX}" y="{$maxY - $yOffset + 10}" text-anchor="middle" >
	    <xsl:value-of select="$currentX - $xOffset" />
	</svg-text>

	<xsl:if test="$currentX &lt; ( $maxX + $xOffset )">
	    <xsl:call-template name="xAxisLabels">
		<xsl:with-param name="maxX" select="$maxX" />
		<xsl:with-param name="maxY" select="$maxY" />
		<xsl:with-param name="xOffset" select="$xOffset" />
		<xsl:with-param name="yOffset" select="$yOffset" />
		<xsl:with-param name="interval" select="$interval" />
		<xsl:with-param name="currentX" select="$currentX + $interval" />
	    </xsl:call-template>
	</xsl:if>
    </xsl:template>
    
    <xsl:output method="html"/>

    <xsl:param name="maxX" select="count(/dinosaurs/dinosaur)*100" />
    <xsl:param name="yOffset" select="120" />
    <xsl:param name="xOffset" select="100" />

    <xsl:template match="dinosaurs" mode="svgforweights">
	<xsl:param name="maxY">400</xsl:param>
	<h2>Dinosaur weights</h2>

	<!-- The +1 for maxX allows the last line to be displayed -->
	<svg-svg width="400px" height="400px" viewBox="0 0 {$maxX + $xOffset} {$maxY}" preserveAspectRatio="none">
	    <!-- Stylesheets, and styles in general, don't seem to work with inline SVG -->
	    <xsl:call-template name="graphStyles" />
	    <svg-defs>
		<xsl:call-template name="graphFilters" />
	    </svg-defs>
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
	</svg-svg>
    </xsl:template>

   <xsl:template match="dinosaurs" mode="svgforheights">
	<xsl:param name="maxY">300</xsl:param>
	<h2>Dinosaur heights</h2>
	<!-- The +1 for maxX allows the last line to be displayed -->
	<svg-svg width="400px" height="400px" viewBox="0 0 {$maxX + $xOffset} {$maxY}" preserveAspectRatio="none">
	    <!-- Stylesheets, and styles in general, don't seem to work with inline SVG -->
	    <xsl:call-template name="graphStyles" />
	    <svg-defs>
		<xsl:call-template name="graphFilters" />
	    </svg-defs>
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
	</svg-svg>
    </xsl:template>

    <xsl:template match="dinosaurs">
	<html xmlns-svg="http://www.w3.org/2000/svg">

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
