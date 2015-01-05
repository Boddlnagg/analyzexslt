<?xml version="1.0" ?>
<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
    xmlns:svg="http://www.w3.org/2000/svg" >

    <xsl:template name="svgDisplayerInclude">
	<object id="AdobeSVG" CLASSID="clsid:78156a80-c6a1-4bbf-8e6a-3cd390eeb4e2"></object>
	<xsl:processing-instruction name = "import" > namespace="svg" implementation="#AdobeSVG"</xsl:processing-instruction> 
    </xsl:template>

    <xsl:template name="graphStyles">
	<!-- Note - styles don't appear to work with the Adobe SVG plugin
	    when the SVG is included inline as part of an XHTML document
	-->
	<svg:style type="text/css"><![CDATA[
	    text { fill:black; font-size:12; font-family:Arial }
	    .gridline { stroke: red; stroke-opacity: 0.2; }
	    .axis { stroke: blue; stroke-opacity: 1.0; }
	    .bar { stroke:rgb(128,128,255); stroke-width:1; filter:url(#Drop_Shadow) }
	    .barLabel { writing-mode: tb; font-size: 12; }
	    .axisLabel { text-anchor: end; }
	    .title { text-anchor: middle; font-size: 24; }
	]]></svg:style>
    </xsl:template>

    <xsl:template name="graphFilters">
	<svg:filter id="Drop_Shadow" filterUnits="objectBoundingBox" x="-10%" y="-10%"
		width="150%" height="150%">
		<svg:feGaussianBlur in="SourceAlpha" stdDeviation="3" result="blurredAlpha"/>
		<svg:feOffset in="blurredAlpha" dx="3" dy="3" result="offsetBlurredAlpha"/>
		<svg:feFlood result="flooded" style="flood-color:rgb(0,0,0);flood-opacity:0.65"/>
		<svg:feComposite in="flooded" operator="in" in2="offsetBlurredAlpha"
			result="coloredShadow"/>
		<svg:feComposite in="SourceGraphic" in2="coloredShadow" operator="over"/>
	</svg:filter>
    </xsl:template>

    <xsl:template name="drawBar" >
	    <xsl:param name="height_condition" />
	    <xsl:param name="bar_width" >60</xsl:param>
	    <xsl:param name="bar_separation">20</xsl:param>
	    <xsl:param name="maxY" />
	    <xsl:param name="yOffset" />
    
	    <svg:rect x="{ (position() * ($bar_width + ($bar_separation * 2))) + ($bar_separation) }" y="{$maxY - $yOffset - $height_condition}" 
		      width="{$bar_width}" height="{$height_condition}" 

		      class="bar" fill="rgb(128,128,255)" filter="url(#Drop_Shadow)" />
<!--
	    Script doesn't work when SVG is included using the
	    svg namespace within an XHTML document with the Adobe plugin.
		onmouseover="evt.getTarget().setAttribute('width', 50)"
		onmouseout="evt.getTarget().setAttribute('width', 26)"
-->

	    <svg:text x="{ ($bar_width div 2) +(position() * ($bar_width + ($bar_separation * 2))) + ($bar_separation) }" 
		    y="{$maxY - $yOffset + 5}" writing-mode="tb" 
		    class="barLabel">
		<xsl:value-of select="@name"/>
	    </svg:text>
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
	
	<svg:line class="axis" stroke="blue" x1="{$xOffset}" y1="{$maxY - $yOffset}" 
		    x2="{$maxX + $xOffset}" y2="{$maxY - $yOffset}" />

	<svg:text x="{$xOffset + ($maxX div 2)}"
		y="{$maxY - ($yOffset div 2)}"
		class="axisLabel">
	    <xsl:value-of select="$xAxisTitle"/>
	</svg:text>

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

	<svg:line class="axis" stroke="blue" x1="{$xOffset}" y1="0" 
		    x2="{$xOffset}" y2="{$maxY - $yOffset}" />

	<svg:text x="{$xOffset div 2}"
		y="{($maxY div 2) - $yOffset}" writing-mode="tb" 
		class="axisLabel">
	    <xsl:value-of select="$yAxisTitle"/>
	</svg:text>


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

	<svg:text class="title" x="{$xOffset + ($maxX div 2)}" y="{$yOffset div 2}" text-anchor="middle">
	    <xsl:value-of select="$title"/>
	</svg:text>
    </xsl:template>

    <xsl:template name="yAxisLabels">
	<xsl:param name="maxX" />
	<xsl:param name="maxY" />
	<xsl:param name="interval" />
	<xsl:param name="xOffset" />
	<xsl:param name="yOffset" />
	<xsl:param name="currentY">0</xsl:param>

	<svg:text x="{$xOffset}"
		y="{$maxY - $currentY - $yOffset }"
		text-anchor="end"
		class="axisLabel">
	    <xsl:value-of select="$currentY"/>
	</svg:text>

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

	<svg:line class="gridline" x1="{$currentX}" y1="0" x2="{$currentX}" y2="{$maxY - $yOffset}" />	

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

	<svg:line class="gridline" x1="{$xOffset}" y1="{$currentY - $yOffset}" x2="{$maxX + $xOffset}" y2="{$currentY - $yOffset}" />	

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

	<svg:text class="axisLabel" x="{$currentX}" y="{$maxY - $yOffset + 10}" text-anchor="middle" >
	    <xsl:value-of select="$currentX - $xOffset" />
	</svg:text>

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

</xsl:stylesheet>
