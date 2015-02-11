<?xml version="1.0" encoding="ISO-8859-1"?>

<xsl:stylesheet version="1.0"
xmlns:xsl="http://www.w3.org/1999/XSL/Transform">

<xsl:template match="/">
    <xsl:apply-templates select="book"/>
</xsl:template>

<xsl:template match="/book">

<html>
<head>
	<link href="xml.css" rel="stylesheet" type="text/css" />
</head>
<body>


<xsl:for-each select="metadata/identifier"> <!-- removed predicate [format='epub3'] -->

	<h1>toc.xhtml for <xsl:value-of select="type"/> version</h1>
	<p class="first">&lt;?xml version="1.0" encoding="utf-8"?&gt;</p>
	<p class="first">&lt;html xmlns="http://www.w3.org/1999/xhtml" xmlns:epub="http://www.idpf.org/2007/ops"&gt;</p>
		<p class="second">&lt;head&gt;</p>
			<p class="third">&lt;title&gt;toc.xhtml&lt;/title&gt;</p>
			<p class="third">&lt;link href="template.css" rel="stylesheet" type="text/css" /&gt;</p>
		<p class="second">&lt;/head&gt;</p>
	<p class="first">&lt;body&gt;</p>
	
		<p class="second">&lt;nav id="toc" epub:type="toc"&gt;</p>
			<p class="third">&lt;h1&gt;Contents&lt;/h1&gt;</p>
			<p class="third">&lt;ol&gt;</p>
			<xsl:for-each select="/book/items/file">
				<xsl:if test="toc='yes' and version=/book/metadata/identifier/type">
					<p class="fourth">&lt;li id="<xsl:value-of select="id"/>"&gt;&lt;a href="<xsl:value-of select="src"/>"&gt;<xsl:value-of select="toc-text"/>&lt;/a&gt;&lt;/li&gt;</p>
				</xsl:if>
			</xsl:for-each>
			<p class="third">&lt;/ol&gt;</p>
		<p class="second">&lt;/nav&gt;</p>
	<p class="first">&lt;/body&gt;</p>
	<p class="first">&lt;/html&gt;</p>
		
		
<hr/>

	<h1>toc.ncx for <xsl:value-of select="type"/> version</h1>
	<p class="first">&lt;?xml version="1.0"?&gt;</p>
	<p class="first">&lt;!DOCTYPE ncx PUBLIC "-//NISO//DTD ncx 2005-1//EN" "http://www.daisy.org/z3986/2005/ncx-2005-1.dtd"&gt;</p>
	<p class="first">&lt;ncx xmlns="http://www.daisy.org/z3986/2005/ncx/" version="2005-1"&gt;</p>

	<p class="first">&lt;head&gt;</p>
	  <p class="second">&lt;meta name="dtb:uid" content=""/&gt;</p>
	  <p class="second">&lt;meta name="dtb:depth" content="1"/&gt;</p>
	  <p class="second">&lt;meta name="dtb:totalPageCount" content="0"/&gt;</p>
	  <p class="second">&lt;meta name="dtb:maxPageNumber" content="0"/&gt;</p>
	<p class="first">&lt;/head&gt;</p>

	<p class="first">&lt;docTitle&gt;</p>
		<p class="second">&lt;text&gt;<xsl:value-of select="metadata/title"/>&lt;/text&gt;</p>
	<p class="first">&lt;/docTitle&gt;</p>
	<p class="first">&lt;navMap&gt;</p>
	<xsl:for-each select="/book/items/file">
		<xsl:if test="toc='yes' and version=/book/metadata/identifier/type">
			<p class="second">&lt;navPoint id="<xsl:value-of select="id"/>" playOrder="<xsl:number count="file"/>"&gt;</p>
			<p class="third">&lt;navLabel&gt;</p>
			<p class="fourth">&lt;text&gt;<xsl:value-of select="toc-text"/>&lt;/text&gt;</p>
			<p class="third">&lt;/navLabel&gt;</p>
			<p class="third">&lt;content src="<xsl:value-of select="src"/>"/&gt;</p>
			<p class="second">&lt;/navPoint&gt;</p>
		</xsl:if>
	</xsl:for-each>
	<p class="first">&lt;/navMap&gt;</p>
	<p class="first">&lt;/ncx&gt;</p>
	
<hr/>

	<h1>content.opf for <xsl:value-of select="type"/> version</h1>

	<p class="first">&lt;?xml version="1.0"?&gt;</p>
	<p class="first">&lt;package xmlns="http://www.idpf.org/2007/opf" xmlns:dc="http://purl.org/dc/elements/1.1/" unique-identifier="db-id" version="3.0"&gt;</p>

	<p class="first">&lt;metadata&gt;</p>

	<xsl:for-each select="/book/metadata/title">
		<xsl:choose>
			<xsl:when test="sequence != ''"> <!-- This checks to see if there is a sequence defined. If so, it uses those numbers, and if not, it assignes them -->
				<p class="second">&lt;dc:title id="t<xsl:value-of select="sequence"/>"&gt;<xsl:value-of select="text"/>&lt;/dc:title&gt;</p>
				<xsl:choose>
					<xsl:when test="type != ''">
						<p class="third">&lt;meta refines="#t<xsl:value-of select="sequence"/>" property="title-type"&gt; <xsl:value-of select="type"/>&lt;/meta&gt;</p>
					</xsl:when>
					<xsl:otherwise>
						<p class="third title-type-missing">All titles must have a unique type declared</p>
					</xsl:otherwise>
				</xsl:choose>
				<p class="third">&lt;meta refines="#t<xsl:value-of select="sequence"/>" property="display-seq"&gt;<xsl:value-of select="sequence"/>&lt;/meta&gt;</p>
				<xsl:if test="volume != ''">
					<p class="third">&lt;meta refines="#t<xsl:value-of select="sequence"/>" property="group-position"&gt;<xsl:value-of select="volume"/>&lt;/meta&gt;</p>
				</xsl:if>
			</xsl:when>
			<xsl:otherwise>
				<p class="second">&lt;dc:title id="t<xsl:number count="title"/>"&gt;<xsl:value-of select="text"/>&lt;/dc:title&gt;</p>
				<xsl:choose>
					<xsl:when test="type != ''">
						<p class="third">&lt;meta refines="#t<xsl:number count="title"/>" property="title-type"&gt; <xsl:value-of select="type"/>&lt;/meta&gt;</p>
					</xsl:when>
					<xsl:otherwise>
						<p class="third title-type-missing">All titles must have a unique type declared</p>
					</xsl:otherwise>
				</xsl:choose>
				<p class="third">&lt;meta refines="#t<xsl:number count="title"/>" property="display-seq"&gt;<xsl:number count="title"/>&lt;/meta&gt;</p>
				<xsl:if test="volume != ''">
					<p class="third">&lt;meta refines="#t<xsl:number count="title"/>" property="group-position"&gt;<xsl:value-of select="volume"/>&lt;/meta&gt;</p>
				</xsl:if>
			</xsl:otherwise>
		</xsl:choose>
	</xsl:for-each>

	<xsl:for-each select="/book/metadata/creator">
		<xsl:choose>
			<xsl:when test="sequence != ''">
				<p class="second">&lt;dc:creator id="creator<xsl:value-of select="sequence"/>"&gt;<xsl:value-of select="text"/>&lt;/dc:creator&gt;</p>
				<xsl:if test="role !=''">
					<p class="third">&lt;meta refines="#t<xsl:value-of select="sequence"/>" property="role"&gt;<xsl:value-of select="role"/>&lt;/meta&gt;</p>
				</xsl:if>
				<xsl:if test="sequence !=''">
					<p class="third">&lt;meta refines="#t<xsl:value-of select="sequence"/>" property="display-seq"&gt;<xsl:value-of select="sequence"/>&lt;/meta&gt;</p>
				</xsl:if>
				<xsl:if test="file-as !=''">
					<p class="third">&lt;meta refines="#t<xsl:value-of select="sequence"/>" property="file-as"&gt;<xsl:value-of select="file-as"/>&lt;/meta&gt;</p>
				</xsl:if>
			</xsl:when>
			<xsl:otherwise>
				<p class="second">&lt;dc:creator id="creator<xsl:number count="creator"/>"&gt;<xsl:value-of select="text"/>&lt;/dc:creator&gt;</p>
				<xsl:if test="role !=''">
					<p class="third">&lt;meta refines="#t<xsl:number count="creator"/>" property="role"&gt;<xsl:value-of select="role"/>&lt;/meta&gt;</p>
				</xsl:if>
				<xsl:if test="sequence !=''">
					<p class="third">&lt;meta refines="#t<xsl:number count="creator"/>" property="display-seq"&gt;<xsl:number count="title"/>&lt;/meta&gt;</p>
				</xsl:if>
				<xsl:if test="file-as !=''">
					<p class="third">&lt;meta refines="#t<xsl:number count="creator"/>" property="file-as"&gt;<xsl:value-of select="file-as"/>&lt;/meta&gt;</p>
				</xsl:if>
			
			</xsl:otherwise>
		</xsl:choose>
	</xsl:for-each>

	<p class="second">&lt;dc:contributor id="digitalbindery"&gt;Digital Bindery&lt;/dc:contributor&gt;</p>
	<p class="third">&lt;meta refines="#digitalbindery" property="role" scheme="marc:relators" id="role"&gt;bkd&lt;/meta&gt;</p>

	<p class="second">&lt;dc:subject&gt;<xsl:value-of select="/book/metadata/subject"/>&lt;/dc:subject&gt;</p>
	<p class="second">&lt;dc:description&gt;<xsl:value-of select="/book/metadata/description"/>&lt;/dc:description&gt;</p>
	<p class="second">&lt;dc:publisher&gt;<xsl:value-of select="/book/metadata/publisher"/>&lt;/dc:publisher&gt;</p>
	<p class="second">&lt;dc:rights&gt;<xsl:value-of select="/book/metadata/rights"/>&lt;/dc:rights&gt;</p>
	<p class="second">&lt;dc:identifier id="db-id"&gt;<xsl:value-of select="number"/>&lt;/dc:identifier&gt;</p>
	<p class="second">&lt;meta property="dcterms:modified"&gt;<xsl:value-of select="/book/metadata/mod-date"/>T<xsl:value-of select="/book/metadata/mod-time"/>Z&lt;/meta&gt;</p> 
	<p class="second">&lt;meta name="cover" content="<xsl:for-each select="/book/items/file"><xsl:if test="cover-img='yes'"><xsl:value-of select="id"/></xsl:if></xsl:for-each>" /&gt;</p> 
	<p class="second">&lt;dc:language&gt;<xsl:value-of select="/book/metadata/language"/>&lt;/dc:language&gt;</p>
	<p class="first">&lt;/metadata&gt;</p>

	<p class="first">&lt;manifest&gt;</p>
	<xsl:for-each select="/book/items/file"><xsl:if test="extension != 'id' and version=/book/metadata/identifier/type">
		<p class="second">&lt;item id="<xsl:value-of select="id"/>" <xsl:choose>
			<xsl:when test="nav='yes'">properties="nav<xsl:if test="cover-img='yes'"> cover-image</xsl:if><xsl:if test="math='yes'"> mathml</xsl:if><xsl:if test="remote='yes'"> remote-resources</xsl:if><xsl:if test="script='yes'"> scripted</xsl:if><xsl:if test="svg='yes'"> svg</xsl:if><xsl:if test="switch='yes'"> switch</xsl:if>"</xsl:when>
			<xsl:when test="cover-img='yes'">properties="cover-image<xsl:if test="math='yes'"> mathml</xsl:if><xsl:if test="remote='yes'"> remote-resources</xsl:if><xsl:if test="script='yes'"> scripted</xsl:if><xsl:if test="svg='yes'"> svg</xsl:if><xsl:if test="switch='yes'"> switch</xsl:if>"</xsl:when>
			<xsl:when test="math='yes'">properties="mathml<xsl:if test="remote='yes'"> remote-resources</xsl:if><xsl:if test="script='yes'"> scripted</xsl:if><xsl:if test="svg='yes'"> svg</xsl:if><xsl:if test="switch='yes'"> switch</xsl:if>"</xsl:when>
			<xsl:when test="remote='yes'">properties="remote-resources<xsl:if test="script='yes'"> scripted</xsl:if><xsl:if test="svg='yes'"> svg</xsl:if><xsl:if test="switch='yes'"> switch</xsl:if>"</xsl:when>
			<xsl:when test="script='yes'">properties="scripted<xsl:if test="svg='yes'"> svg</xsl:if><xsl:if test="switch='yes'"> switch</xsl:if>"</xsl:when>
			<xsl:when test="svg='yes'">properties="svg<xsl:if test="switch='yes'"> switch</xsl:if>"</xsl:when>
			<xsl:when test="switch='yes'">properties="switch"</xsl:when>
			<xsl:otherwise/>
			</xsl:choose>
			
		href="<xsl:value-of select="src"/>" media-type="<xsl:choose>
			<xsl:when test="extension='xhtml'">application/xhtml+xml</xsl:when>
			<xsl:when test="extension='ncx'">application/x-dtbncx+xml</xsl:when>
			<xsl:when test="extension='css'">text/css</xsl:when>
			<xsl:when test="extension='jpg'">image/jpeg</xsl:when>
			<xsl:when test="extension='jpeg'">image/jpeg</xsl:when>
			<xsl:when test="extension='png'">image/png</xsl:when>
			<xsl:when test="extension='png'">application/xhtml+xml</xsl:when>
			<xsl:when test="extension='gif'">application/xhtml+xml</xsl:when>
			<xsl:when test="extension='ttf'">application/x-font-truetype</xsl:when>
			<xsl:when test="extension='mov'">video/quicktime</xsl:when>
			<xsl:when test="extension='mp4'">video/h264</xsl:when>
			<xsl:when test="extension='mp3'">audio/mp3</xsl:when>
			<xsl:when test="extension='swf'">application/x-shockwave-flash</xsl:when>
			<xsl:when test="extension='tif'">image/tiff</xsl:when>
			<xsl:when test="extension='tiff'">image/tiff</xsl:when>
			<xsl:when test="extension='txt'">text/plain</xsl:when>
			<xsl:when test="extension='gif'">image/gif</xsl:when>
			<xsl:when test="extension='eot'">application/vnd.ms-fontobject</xsl:when>
			<xsl:when test="extension='otf'">application/octet-stream</xsl:when>
			<xsl:when test="extension='woff'">application/font-woff</xsl:when>
			<xsl:when test="extension='svg'">image/svg+xml</xsl:when>
		</xsl:choose>"/&gt;</p></xsl:if>
	</xsl:for-each>
	<p class="first">&lt;/manifest&gt;</p>

	<p class="first">&lt;spine toc="ncx"&gt;</p>
	<xsl:for-each select="/book/items/file">
		<xsl:if test="spine='yes' and version=/book/metadata/identifier/type">
			<p class="second">&lt;itemref idref="<xsl:value-of select="id"/>"/&gt;</p>
		</xsl:if>
	</xsl:for-each>
	<p class="first">&lt;/spine&gt;</p>
	<p class="first">&lt;/package&gt;</p>
</xsl:for-each>

<xsl:for-each select="metadata/identifier"> <!-- removed predicate [format='mobi'] -->
<hr/>
<hr/>
	<h1>toc.xhtml for <xsl:value-of select="type"/> version</h1>
	<p class="first">&lt;?xml version="1.0" encoding="utf-8"?&gt;</p>
	<p class="first">&lt;html xmlns="http://www.w3.org/1999/xhtml" xmlns:epub="http://www.idpf.org/2007/ops"&gt;</p>
		<p class="second">&lt;head&gt;</p>
			<p class="third">&lt;title&gt;toc.xhtml&lt;/title&gt;</p>
			<p class="third">&lt;link href="template.css" rel="stylesheet" type="text/css" /&gt;</p>
		<p class="second">&lt;/head&gt;</p>
	<p class="first">&lt;body&gt;</p>
	
			<p class="second">&lt;h1&gt;Contents&lt;/h1&gt;</p>
			<xsl:for-each select="/book/items/file">
				<xsl:if test="toc='yes' and version=/book/metadata/identifier/type">
					<p class="second">&lt;p class="toc" id="<xsl:value-of select="id"/>"&gt;&lt;a href="<xsl:value-of select="src"/>"&gt;<xsl:value-of select="toc-text"/>&lt;/a&gt;&lt;/p&gt;</p>
				</xsl:if>
			</xsl:for-each>
	<p class="first">&lt;/body&gt;</p>
	<p class="first">&lt;/html&gt;</p>

<hr />
	<h1>toc.ncx for <xsl:value-of select="type"/> version</h1>
	<p class="first">&lt;?xml version="1.0"?&gt;</p>
	<p class="first">&lt;!DOCTYPE ncx PUBLIC "-//NISO//DTD ncx 2005-1//EN" "http://www.daisy.org/z3986/2005/ncx-2005-1.dtd"&gt;</p>
	<p class="first">&lt;ncx xmlns="http://www.daisy.org/z3986/2005/ncx/" version="2005-1"&gt;</p>

	<p class="first">&lt;head&gt;</p>
	  <p class="second">&lt;meta name="dtb:uid" content=""/&gt;</p>
	  <p class="second">&lt;meta name="dtb:depth" content="1"/&gt;</p>
	  <p class="second">&lt;meta name="dtb:totalPageCount" content="0"/&gt;</p>
	  <p class="second">&lt;meta name="dtb:maxPageNumber" content="0"/&gt;</p>
	<p class="first">&lt;/head&gt;</p>

	<p class="first">&lt;docTitle&gt;</p>
		<p class="second">&lt;text&gt;<xsl:value-of select="metadata/title"/>&lt;/text&gt;</p>
	<p class="first">&lt;/docTitle&gt;</p>
	<p class="first">&lt;navMap&gt;</p>
	<xsl:for-each select="/book/items/file">
		<xsl:if test="toc='yes' and version=/book/metadata/identifier/type">
			<p class="second">&lt;navPoint id="<xsl:value-of select="id"/>" playOrder="<xsl:number count="file"/>"&gt;</p>
			<p class="third">&lt;navLabel&gt;</p>
			<p class="fourth">&lt;text&gt;<xsl:value-of select="toc-text"/>&lt;/text&gt;</p>
			<p class="third">&lt;/navLabel&gt;</p>
			<p class="third">&lt;content src="<xsl:value-of select="src"/>"/&gt;</p>
			<p class="second">&lt;/navPoint&gt;</p>
		</xsl:if>
	</xsl:for-each>
	<p class="first">&lt;/navMap&gt;</p>
	<p class="first">&lt;/ncx&gt;</p>
	
<hr/>

	<h1>content.opf for <xsl:value-of select="type"/> version</h1>

	<p class="first">&lt;?xml version="1.0"?&gt;</p>
	<p class="first">&lt;package xmlns="http://www.idpf.org/2007/opf" xmlns:dc="http://purl.org/dc/elements/1.1/" unique-identifier="db-id" version="3.0"&gt;</p>

	<p class="first">&lt;metadata&gt;</p>
	<xsl:for-each select="/book/metadata/title">
		<xsl:choose>
			<xsl:when test="sequence != ''"> <!-- This checks to see if there is a sequence defined. If so, it uses those numbers, and if not, it assignes them -->
				<p class="second">&lt;dc:title id="t<xsl:value-of select="sequence"/>"&gt;<xsl:value-of select="text"/>&lt;/dc:title&gt;</p>
				<xsl:choose>
					<xsl:when test="type != ''">
						<p class="third">&lt;meta refines="#t<xsl:value-of select="sequence"/>" property="title-type"&gt; <xsl:value-of select="type"/>&lt;/meta&gt;</p>
					</xsl:when>
					<xsl:otherwise>
						<p class="third title-type-missing">All titles must have a unique type declared</p>
					</xsl:otherwise>
				</xsl:choose>
				<p class="third">&lt;meta refines="#t<xsl:value-of select="sequence"/>" property="display-seq"&gt;<xsl:value-of select="sequence"/>&lt;/meta&gt;</p>
				<xsl:if test="volume != ''">
					<p class="third">&lt;meta refines="#t<xsl:value-of select="sequence"/>" property="group-position"&gt;<xsl:value-of select="volume"/>&lt;/meta&gt;</p>
				</xsl:if>
			</xsl:when>
			<xsl:otherwise>
				<p class="second">&lt;dc:title id="t<xsl:number count="title"/>"&gt;<xsl:value-of select="text"/>&lt;/dc:title&gt;</p>
				<xsl:choose>
					<xsl:when test="type != ''">
						<p class="third">&lt;meta refines="#t<xsl:number count="title"/>" property="title-type"&gt; <xsl:value-of select="type"/>&lt;/meta&gt;</p>
					</xsl:when>
					<xsl:otherwise>
						<p class="third title-type-missing">All titles must have a unique type declared</p>
					</xsl:otherwise>
				</xsl:choose>
				<p class="third">&lt;meta refines="#t<xsl:number count="title"/>" property="display-seq"&gt;<xsl:number count="title"/>&lt;/meta&gt;</p>
				<xsl:if test="volume != ''">
					<p class="third">&lt;meta refines="#t<xsl:number count="title"/>" property="group-position"&gt;<xsl:value-of select="volume"/>&lt;/meta&gt;</p>
				</xsl:if>
			</xsl:otherwise>
		</xsl:choose>
	</xsl:for-each>

	<xsl:for-each select="/book/metadata/creator">
		<xsl:choose>
			<xsl:when test="sequence != ''">
				<p class="second">&lt;dc:creator id="creator<xsl:value-of select="sequence"/>"&gt;<xsl:value-of select="text"/>&lt;/dc:creator&gt;</p>
				<xsl:if test="role !=''">
					<p class="third">&lt;meta refines="#t<xsl:value-of select="sequence"/>" property="role"&gt;<xsl:value-of select="role"/>&lt;/meta&gt;</p>
				</xsl:if>
				<xsl:if test="sequence !=''">
					<p class="third">&lt;meta refines="#t<xsl:value-of select="sequence"/>" property="display-seq"&gt;<xsl:value-of select="sequence"/>&lt;/meta&gt;</p>
				</xsl:if>
				<xsl:if test="file-as !=''">
					<p class="third">&lt;meta refines="#t<xsl:value-of select="sequence"/>" property="file-as"&gt;<xsl:value-of select="file-as"/>&lt;/meta&gt;</p>
				</xsl:if>
			</xsl:when>
			<xsl:otherwise>
				<p class="second">&lt;dc:creator id="creator<xsl:number count="creator"/>"&gt;<xsl:value-of select="text"/>&lt;/dc:creator&gt;</p>
				<xsl:if test="role !=''">
					<p class="third">&lt;meta refines="#t<xsl:number count="creator"/>" property="role"&gt;<xsl:value-of select="role"/>&lt;/meta&gt;</p>
				</xsl:if>
				<xsl:if test="sequence !=''">
					<p class="third">&lt;meta refines="#t<xsl:number count="creator"/>" property="display-seq"&gt;<xsl:number count="title"/>&lt;/meta&gt;</p>
				</xsl:if>
				<xsl:if test="file-as !=''">
					<p class="third">&lt;meta refines="#t<xsl:number count="creator"/>" property="file-as"&gt;<xsl:value-of select="file-as"/>&lt;/meta&gt;</p>
				</xsl:if>
			
			</xsl:otherwise>
		</xsl:choose>
	</xsl:for-each>

	<p class="second">&lt;dc:contributor id="digitalbindery"&gt;Digital Bindery&lt;/dc:contributor&gt;</p>
	<p class="third">&lt;meta refines="#digitalbindery" property="role" scheme="marc:relators" id="role"&gt;bkd&lt;/meta&gt;</p>
	<p class="second">&lt;dc:subject&gt;<xsl:value-of select="/book/metadata/subject"/>&lt;/dc:subject&gt;</p>
	<p class="second">&lt;dc:description&gt;<xsl:value-of select="/book/metadata/description"/>&lt;/dc:description&gt;</p>
	<p class="second">&lt;dc:publisher&gt;<xsl:value-of select="/book/metadata/publisher"/>&lt;/dc:publisher&gt;</p>
	<p class="second">&lt;dc:rights&gt;<xsl:value-of select="/book/metadata/rights"/>&lt;/dc:rights&gt;</p>
	<p class="second">&lt;dc:identifier id="db-id"&gt;<xsl:value-of select="number"/>&lt;/dc:identifier&gt;</p>
	<p class="second">&lt;meta property="dcterms:modified"&gt;<xsl:value-of select="/book/metadata/mod-date"/>T<xsl:value-of select="/book/metadata/mod-time"/>Z&lt;/meta&gt;</p>
	<p class="second">&lt;meta name="cover" content="<xsl:for-each select="/book/items/file"><xsl:if test="cover-img='yes'"><xsl:value-of select="id"/></xsl:if></xsl:for-each>" /&gt;</p> 
	<p class="second">&lt;dc:language&gt;<xsl:value-of select="/book/metadata/language"/>&lt;/dc:language&gt;</p>
	<p class="first">&lt;/metadata&gt;</p>

	<p class="first">&lt;manifest&gt;</p>
	<xsl:for-each select="/book/items/file"><xsl:if test="extension != 'id' and version=/book/metadata/identifier/type">
		<p class="second">&lt;item id="<xsl:value-of select="id"/>" href="<xsl:value-of select="src"/>" media-type="<xsl:choose>
			<xsl:when test="extension='xhtml'">application/xhtml+xml</xsl:when>
			<xsl:when test="extension='ncx'">application/x-dtbncx+xml</xsl:when>
			<xsl:when test="extension='css'">text/css</xsl:when>
			<xsl:when test="extension='jpg'">image/jpeg</xsl:when>
			<xsl:when test="extension='jpeg'">image/jpeg</xsl:when>
			<xsl:when test="extension='png'">image/png</xsl:when>
			<xsl:when test="extension='png'">application/xhtml+xml</xsl:when>
			<xsl:when test="extension='gif'">application/xhtml+xml</xsl:when>
			<xsl:when test="extension='ttf'">application/x-font-truetype</xsl:when>
			<xsl:when test="extension='mov'">video/quicktime</xsl:when>
			<xsl:when test="extension='mp4'">video/h264</xsl:when>
			<xsl:when test="extension='mp3'">audio/mp3</xsl:when>
			<xsl:when test="extension='swf'">application/x-shockwave-flash</xsl:when>
			<xsl:when test="extension='tif'">image/tiff</xsl:when>
			<xsl:when test="extension='tiff'">image/tiff</xsl:when>
			<xsl:when test="extension='txt'">text/plain</xsl:when>
			<xsl:when test="extension='gif'">image/gif</xsl:when>
			<xsl:when test="extension='eot'">application/vnd.ms-fontobject</xsl:when>
			<xsl:when test="extension='otf'">application/octet-stream</xsl:when>
			<xsl:when test="extension='woff'">application/font-woff</xsl:when>
			<xsl:when test="extension='svg'">image/svg+xml</xsl:when>
		</xsl:choose>"/&gt;</p></xsl:if>
	</xsl:for-each>
	<p class="first">&lt;/manifest&gt;</p>

	<p class="first">&lt;spine toc="ncx"&gt;</p>
	<xsl:for-each select="/book/items/file">
		<xsl:if test="spine='yes' and version=/book/metadata/identifier/type">
			<p class="second">&lt;itemref idref="<xsl:value-of select="id"/>"/&gt;</p>
		</xsl:if>
	</xsl:for-each>
	<p class="first">&lt;/spine&gt;</p>
	<p class="first">&lt;guide&gt;</p>

	<p class="second">&lt;reference type="toc" title="Contents" href="<xsl:for-each select="/book/items/file"><xsl:if test="nav='yes'"><xsl:value-of select="src"/></xsl:if></xsl:for-each>"/&gt;</p>

	<p class="first">&lt;/guide&gt;</p>
	<p class="first">&lt;/package&gt;</p>

</xsl:for-each>

</body>
</html>
</xsl:template>

</xsl:stylesheet>