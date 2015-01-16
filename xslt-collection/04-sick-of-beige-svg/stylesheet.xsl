<?xml version="1.0" encoding="UTF-8"?>
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" version="1.0" xmlns:dc="http://purl.org/dc/elements/1.1/" xmlns:cc="http://creativecommons.org/ns#" xmlns:rdf="http://www.w3.org/1999/02/22-rdf-syntax-ns#" xmlns="http://www.w3.org/2000/svg" xmlns:xlink="http://www.w3.org/1999/xlink" xmlns:inkscape="http://www.inkscape.org/namespaces/inkscape">
	<xsl:output method="xml" omit-xml-declaration="yes"/>

	<xsl:param name="width" select="100"/>
	<xsl:param name="height" select="80"/>
	<xsl:param name="dp-model" select="concat('DP', $width, $height)"/>
	<xsl:param name="corner-radius" select="4"/>
	<xsl:param name="keepout" select="6"/>
	<xsl:param name="hole-diameter" select="3.2"/>
	<xsl:param name="edge-keepout" select="1.7"/>
	<xsl:param name="usb-center-from-edge" select="5.25"/>
	<xsl:param name="center-mark-size" select="2"/>
	<xsl:param name="center-mark-stroke-width" select="0.25"/>

	<xsl:variable name="edge-keepout-radius" select="$keepout div 2"/>
	<xsl:variable name="hole-radius" select="$hole-diameter div 2"/>
	<xsl:variable name="outline-radius" select="$edge-keepout div 2"/>
	<xsl:variable name="ring-diameter" select="$hole-radius + $edge-keepout-radius"/>
	<xsl:variable name="ring-radius" select="$ring-diameter div 2"/>
	<xsl:variable name="ring-stroke-width" select="$edge-keepout-radius - $hole-radius"/>

	<xsl:variable name="bxr" select="$width div 2"/>
	<xsl:variable name="bxl" select="-$bxr"/>
	<xsl:variable name="byb" select="$height div 2"/>
	<xsl:variable name="byt" select="-$byb"/>

	<xsl:variable name="hole-xl" select="$bxl + $corner-radius"/>
	<xsl:variable name="hole-xr" select="$bxr - $corner-radius"/>
	<xsl:variable name="hole-yt" select="$byt + $corner-radius"/>
	<xsl:variable name="hole-yb" select="$byb - $corner-radius"/>

	<xsl:template match="node()|@*">
		<xsl:copy>
			<xsl:apply-templates select="node()|@*"/>
		</xsl:copy>
	</xsl:template>

	<!-- Draw a path for a rounded rectangle about the current location,
		whose corners may have distinct radii, and whose stroke is effectively
		inset from the definition. -->
	<xsl:template name="rounded-rectangle-path">
		<xsl:param name="width"/>
		<xsl:param name="height"/>
		<xsl:param name="left" select="-($width div 2)"/>
		<xsl:param name="top" select="-($height div 2)"/>
		<xsl:param name="stroke-width"/>
		<xsl:param name="radius-tl" select="0"/>
		<xsl:param name="radius-tr" select="0"/>
		<xsl:param name="radius-br" select="0"/>
		<xsl:param name="radius-bl" select="0"/>
		<xsl:param name="style" select="''"/>
		<xsl:param name="id" select="''"/>

		<xsl:variable name="stroke-radius" select="$stroke-width div 2"/>

		<!-- Adjusted for stroke -->
		<xsl:variable name="w" select="$width - $stroke-width"/>
		<xsl:variable name="h" select="$height - $stroke-width"/>
		<xsl:variable name="rtl" select="$radius-tl - $stroke-radius"/>
		<xsl:variable name="rtr" select="$radius-tr - $stroke-radius"/>
		<xsl:variable name="rbr" select="$radius-br - $stroke-radius"/>
		<xsl:variable name="rbl" select="$radius-bl - $stroke-radius"/>

		<xsl:variable name="xl" select="$left + $stroke-radius"/>
		<xsl:variable name="xr" select="$xl + $w"/>
		<xsl:variable name="yt" select="$top + $stroke-radius"/>
		<xsl:variable name="yb" select="$yt + $h"/>

		<path>
			<xsl:if test="string($id) != ''">
				<xsl:attribute name="id">
					<xsl:value-of select="$id"/>
				</xsl:attribute>
			</xsl:if>
			<xsl:attribute name="d">
				<xsl:value-of select="concat('m ', $xl, ' ', $yt + $rtl)"/>

				<xsl:if test="$rtl != 0">
					<xsl:value-of select="concat(' a ', $rtl, ',', $rtl, ' 0 0 1 ', $rtl, ',', -$rtl)"/>
				</xsl:if>

				<xsl:value-of select="concat(' h ', $w - ($rtl + $rtr))"/>

				<xsl:if test="$rtr != 0">
					<xsl:value-of select="concat(' a ', $rtr, ',', $rtr, ' 0 0 1 ', $rtr, ',', $rtr)"/>
				</xsl:if>

				<xsl:value-of select="concat(' v ', $h - ($rtr + $rbr))"/>

				<xsl:if test="$rbr != 0">
					<xsl:value-of select="concat(' a ', $rbr, ',', $rbr, ' 0 0 1 ', -$rbr, ',', $rbr)"/>
				</xsl:if>

				<xsl:value-of select="concat(' h ', -($w - ($rbl + $rbr)))"/>

				<xsl:if test="$rbl != 0">
					<xsl:value-of select="concat(' a ', $rbl, ',', $rbl, ' 0 0 1 ', -$rbl, ',', -$rbl)"/>
				</xsl:if>

				<xsl:value-of select="concat(' v ', -($h - ($rtl + $rbl)))"/>

				<xsl:value-of select="' z'"/>
			</xsl:attribute>
			<xsl:attribute name="style">
				<xsl:if test="number($stroke-width) != 0">
					<xsl:value-of select="concat('stroke-width:', $stroke-width)"/>
					<xsl:if test="normalize-space($style) != ''">;</xsl:if>
				</xsl:if>
				<xsl:value-of select="$style"/>
			</xsl:attribute>
		</path>
	</xsl:template>

	<xsl:template match="/">
		<svg version="1.1" width="{$width}mm" height="{$height}mm" viewBox="{$bxl} {$byt} {$width} {$height}">
			<metadata>
				<rdf:RDF>
					<cc:Work rdf:about="">
						<dc:format>image/svg+xml</dc:format>
						<dc:type rdf:resource="http://purl.org/dc/dcmitype/StillImage"/>
						<dc:title/>
					</cc:Work>
				</rdf:RDF>
			</metadata>
			<defs>
				<g id="center-mark">
					<path d="m 0,{-$center-mark-size div 2} v {$center-mark-size} z" style="stroke:white;stroke-width:{$center-mark-stroke-width}"/>
					<path d="m {-$center-mark-size div 2},0 h {$center-mark-size} z" style="stroke:white;stroke-width:{$center-mark-stroke-width}"/>
				</g>
				<g id="corner-keepout-once">
					<xsl:call-template name="rounded-rectangle-path">
						<xsl:with-param name="width" select="$corner-radius + $edge-keepout-radius"/>
						<xsl:with-param name="height" select="$corner-radius + $edge-keepout-radius"/>
						<xsl:with-param name="left" select="-$corner-radius"/>
						<xsl:with-param name="top" select="-$edge-keepout-radius"/>
						<xsl:with-param name="stroke-width" select="0"/>
						<xsl:with-param name="radius-tl" select="0"/>
						<xsl:with-param name="radius-tr" select="$edge-keepout-radius"/>
						<xsl:with-param name="radius-br" select="0"/>
						<xsl:with-param name="radius-bl" select="$corner-radius"/>
						<xsl:with-param name="style" select="'fill:white;stroke:none'"/>
						<xsl:with-param name="id" select="'corner-keepout-pad'"/>
					</xsl:call-template>
				</g>
				<g id="hole-once">
					<circle cx="0" cy="0" r="{$hole-radius}" style="fill:black;stroke:none"/>
				</g>
			</defs>
			<g inkscape:label="Board" id="layer-board" inkscape:groupmode="layer" style="display:inline">
				<xsl:call-template name="rounded-rectangle-path">
					<xsl:with-param name="width" select="$width"/>
					<xsl:with-param name="height" select="$height"/>
					<xsl:with-param name="stroke-width" select="0"/>
					<xsl:with-param name="radius-tl" select="$corner-radius"/>
					<xsl:with-param name="radius-tr" select="$corner-radius"/>
					<xsl:with-param name="radius-br" select="$corner-radius"/>
					<xsl:with-param name="radius-bl" select="$corner-radius"/>
					<xsl:with-param name="style" select="'fill:#900;stroke:none'"/>
					<xsl:with-param name="id" select="'board'"/>
				</xsl:call-template>
			</g>
			<g inkscape:label="Keepout" id="layer-keepout" inkscape:groupmode="layer" style="display:inline">
				<xsl:call-template name="rounded-rectangle-path">
					<xsl:with-param name="width" select="$width"/>
					<xsl:with-param name="height" select="$height"/>
					<xsl:with-param name="stroke-width" select="$edge-keepout"/>
					<xsl:with-param name="radius-tl" select="$corner-radius"/>
					<xsl:with-param name="radius-tr" select="$corner-radius"/>
					<xsl:with-param name="radius-br" select="$corner-radius"/>
					<xsl:with-param name="radius-bl" select="$corner-radius"/>
					<xsl:with-param name="style" select="'fill:none;stroke:white'"/>
					<xsl:with-param name="id" select="'edge-keepout'"/>
				</xsl:call-template>
				<use transform="translate({$hole-xl},{$hole-yb})" id="lower-left-corner-keepout" x="0" y="0" xlink:href="#corner-keepout-once"/>
				<use transform="matrix(-1,0,0,1,{$hole-xr},{$hole-yb})" id="lower-right-corner-keepout" x="0" y="0" xlink:href="#corner-keepout-once"/>
				<use transform="matrix(-1,0,0,-1,{$hole-xr},{$hole-yt})" id="upper-right-corner-keepout" x="0" y="0" xlink:href="#corner-keepout-once"/>
				<use transform="matrix(1,0,0,-1,{$hole-xl},{$hole-yt})" id="upper-left-corner-keepout" x="0" y="0" xlink:href="#corner-keepout-once"/>
			</g>
			<g inkscape:label="Drill" id="layer-drill" inkscape:groupmode="layer" style="display:inline">
				<use transform="translate({$hole-xl},{$hole-yb})" id="lower-left-hole" x="0" y="0" xlink:href="#hole-once"/>
				<use transform="matrix(-1,0,0,1,{$hole-xr},{$hole-yb})" id="lower-right-hole" x="0" y="0" xlink:href="#hole-once"/>
				<use transform="matrix(-1,0,0,-1,{$hole-xr},{$hole-yt})" id="upper-right-hole" x="0" y="0" xlink:href="#hole-once"/>
				<use transform="matrix(1,0,0,-1,{$hole-xl},{$hole-yt})" id="upper-left-hole" x="0" y="0" xlink:href="#hole-once"/>
			</g>
			<g inkscape:label="Centers" inkscape:groupmode="layer" id="layer-centers" style="display:inline">
				<use id="center-center-mark" x="0" y="0" xlink:href="#center-mark"/>
				<use transform="translate({$bxl + $usb-center-from-edge},0)" id="left-usb-center-mark" x="0" y="0" xlink:href="#center-mark"/>
				<use transform="translate({$bxr - $usb-center-from-edge},0)" id="right-usb-center-mark" x="0" y="0" xlink:href="#center-mark"/>
			</g>
			<g inkscape:label="Description" inkscape:groupmode="layer" id="layer-description" style="display:inline">
				<g id="descriptive-text" transform="translate(0,-6)">
					<text xml:space="preserve" y="-2" style="font-size:4;font-style:normal;font-variant:normal;font-weight:normal;font-stretch:normal;text-align:center;text-anchor:middle;fill:white;fill-opacity:1;font-family:monospace"><tspan><xsl:value-of select="$dp-model"/></tspan></text>
					<text xml:space="preserve" y="2" style="font-size:4;font-style:normal;font-variant:normal;font-weight:normal;font-stretch:normal;text-align:center;text-anchor:middle;fill:white;fill-opacity:1;font-family:monospace"><tspan><xsl:value-of select="concat($width, 'mm × ', $height, 'mm')"/></tspan></text>
				</g>
			</g>
		</svg>
	</xsl:template>
</xsl:stylesheet>
