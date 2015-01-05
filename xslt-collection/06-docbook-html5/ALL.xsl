<?xml version="1.0" encoding="utf-8"?>
<!--

 Author: Mo McRoberts <mo.mcroberts@bbc.co.uk>

 Copyright (c) 2014 BBC

  Licensed under the Apache License, Version 2.0 (the "License");
  you may not use this file except in compliance with the License.
  You may obtain a copy of the License at

      http://www.apache.org/licenses/LICENSE-2.0

  Unless required by applicable law or agreed to in writing, software
  distributed under the License is distributed on an "AS IS" BASIS,
  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
  See the License for the specific language governing permissions and
  limitations under the License.

-->
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" version="1.0" xmlns:xhtml="http://www.w3.org/1999/xhtml" xmlns:db="http://docbook.org/ns" xmlns:xi="http://www.w3.org/2001/XInclude" xmlns:xlink="http://www.w3.org/1999/xlink" exclude-result-prefixes="xhtml db xi xlink">

  	<xsl:output omit-xml-declaration="yes" encoding="utf-8" indent="no" />

	<!--<xsl:include href="block.xsl" />-->
    <!-- <para> -->
	<xsl:template match="//db:para" mode="body">
		<xsl:call-template name="html.block">
			<xsl:with-param name="kind" />
			<xsl:with-param name="element">p</xsl:with-param>
		</xsl:call-template>
	</xsl:template>
	
	<!-- <formalpara> -->
	<xsl:template match="//db:formalpara" mode="body">
		<xsl:call-template name="html.titleblock">
			<xsl:with-param name="kind" />
			<xsl:with-param name="element">p</xsl:with-param>
			<xsl:with-param name="title.element">h2</xsl:with-param>
		</xsl:call-template>
	</xsl:template>

	<!-- <blockquote> -->
	<xsl:template match="//db:blockquote" mode="body">
		<xsl:variable name="attribution"><xsl:copy-of select="db:attribution" /></xsl:variable>
		<blockquote>
			<xsl:apply-templates select="node()" mode="body" />
			<footer><cite><xsl:value-of select="$attribution" /></cite></footer>
		</blockquote>
	</xsl:template>
	<xsl:template match="//db:blockquote/db:attribution" />

	<!-- Admonition elements -->

	<!-- <note> -->
	<xsl:template match="//db:note" mode="body">
		<xsl:call-template name="html.block">
			<xsl:with-param name="kind">info</xsl:with-param>
			<xsl:with-param name="element">aside</xsl:with-param>
		</xsl:call-template>
	</xsl:template>			
	
	<!-- <tip>, <warning>, <important>, <caution> -->
	<xsl:template match="//db:tip|//db:warning|//db:important|//db:caution" mode="body">
		<xsl:call-template name="html.block">
			<xsl:with-param name="element">aside</xsl:with-param>
		</xsl:call-template>
	</xsl:template>			

	<!-- List elements -->

	<!-- <itemizedlist> -->
	<xsl:template match="//db:itemizedlist" mode="body">
	  <ul>
		  <xsl:apply-templates mode="list" />
	  </ul>
	</xsl:template>

	<!-- <orderedlist> -->
	<xsl:template match="//db:orderedlist" mode="body">
	  <ol>
		  <xsl:apply-templates mode="list" />
	  </ol>
	</xsl:template>

	<!-- <variablelist> -->
	<xsl:template match="//db:variablelist" mode="body">
		<dl>
			<xsl:for-each select="db:varlistentry">
				<xsl:apply-templates select="node()" mode="dl" />
			</xsl:for-each>
		</dl>
	</xsl:template>
	
	<xsl:template match="//db:term" mode="dl">
		<dt><xsl:apply-templates select="node()" mode="body" /></dt>
	</xsl:template>
	
	<!-- <listitem> -->
	<xsl:template match="//db:listitem" mode="list">
	  <xsl:choose>
		<xsl:when test="count(*)=1 and count(db:para)=1">
		  <li><xsl:apply-templates select="db:para/node()" mode="body" /></li>
		</xsl:when>
		<xsl:otherwise>
		  <li><xsl:apply-templates select="node()" mode="body" /></li>
		</xsl:otherwise>
	  </xsl:choose>
	</xsl:template>

	<xsl:template match="//db:listitem" mode="dl">
	  <xsl:choose>
		<xsl:when test="count(*)=1 and count(db:para)=1">
		  <dd><xsl:apply-templates select="db:para/node()" mode="body" /></dd>
		</xsl:when>
		<xsl:otherwise>
		  <dd><xsl:apply-templates select="node()" mode="body" /></dd>
		</xsl:otherwise>
	  </xsl:choose>
	</xsl:template>
	
	<!-- <segmentedlist> -->
	<xsl:template match="//db:segmentedlist" mode="body">
		<div class="table">
			<table>
				<thead>
					<tr>
						<xsl:for-each select="db:segtitle">
							<th scope="col">
								<xsl:apply-templates select="node()" mode="body" />
							</th>
						</xsl:for-each>
					</tr>
				</thead>
				<tbody>
					<xsl:for-each select="db:seglistitem">
						<tr>
							<xsl:for-each select="db:seg">
								<td>
									<xsl:apply-templates select="node()" mode="body" />
								</td>
							</xsl:for-each>
						</tr>
					</xsl:for-each>
				</tbody>
			</table>
		</div>
	</xsl:template>

	<!-- <section>, <chapter>, <part> -->
	<xsl:template match="//db:section|//db:chapter|//db:part|//db:refsection|//db:refsect1|//db:refsect2|//db:refsect3|//db:refsect4|//db:reference|//db:refsynopsisdiv|//db:refentry" mode="body">
		<xsl:call-template name="html.titleblock" />
	</xsl:template>
	
	<!-- <simplesect>, <sectN> -->
	<xsl:template match="//db:simplesect|//db:sect1|//db:sect2|//db:sect3|//db:sect4|//db:sect5" mode="body">
		<xsl:call-template name="html.titleblock">
			<xsl:with-param name="kind">section</xsl:with-param>
		</xsl:call-template>
	</xsl:template>
	
	<!-- <refnamediv> -->
	<xsl:template match="//db:refnamediv" mode="body">
	</xsl:template>

	<!-- <example> -->
	<xsl:template match="//db:example" mode="body">
	  <xsl:call-template name="html.titleblock">
		<xsl:with-param name="element">aside</xsl:with-param>
		<xsl:with-param name="kind">example</xsl:with-param>
	  </xsl:call-template>
	</xsl:template>

	<!-- <programlisting> -->
	<xsl:template match="//db:programlisting" mode="body">
	  <xsl:variable name="classes" select="normalize-space(concat('code ', @language, ' ', @role))" />
	  <pre>
		<xsl:attribute name="class"><xsl:value-of select="$classes" /></xsl:attribute>
		<code><xsl:apply-templates select="node()" mode="body" /></code>
	  </pre>
	</xsl:template>

	<!-- <screen> -->
	<xsl:template match="//db:screen" mode="body">
	  <xsl:variable name="classes" select="normalize-space(concat('output ', @role))" />
	  <pre>
		<xsl:attribute name="class"><xsl:value-of select="$classes" /></xsl:attribute>
		<samp><xsl:apply-templates select="node()" mode="body" /></samp>
	  </pre>
	</xsl:template>

	<!-- Titled block output -->
	<xsl:template name="html.titleblock">
		<xsl:param name="element" select="'section'" />
		<xsl:param name="class" />
		<xsl:param name="role" select="@role" />
		<xsl:param name="id" select="normalize-space(@xml:id)" />
		<xsl:param name="kind" select="local-name()" />
		<xsl:param name="title.element" select="'h1'" />
		<xsl:param name="subtitle.element" select="'h2'" />
		<xsl:variable name="classes" select="normalize-space(concat($class, ' ', $role, ' ', $kind))" />
		<xsl:variable name="title">
			<xsl:choose>
				<xsl:when test="db:title">
					<xsl:copy-of select="db:title" />
				</xsl:when>
				<xsl:when test="db:refmeta/db:refentrytitle">
					<xsl:copy-of select="db:refmeta/db:refentrytitle" />
				</xsl:when>
			</xsl:choose>
		</xsl:variable>
		<xsl:variable name="subtitle"><xsl:copy-of select="db:subtitle" /></xsl:variable>
		<xsl:element name="{$element}">
			<xsl:if test="$id != ''"><xsl:attribute name="id"><xsl:value-of select="$id" /></xsl:attribute></xsl:if>
			<xsl:if test="$classes != ''"><xsl:attribute name="class"><xsl:value-of select="$classes" /></xsl:attribute></xsl:if>
			<xsl:if test="normalize-space($title) != ''">
				<xsl:element name="{$title.element}">
					<xsl:for-each select="db:title">
						<xsl:apply-templates select="node()" mode="body" />
					</xsl:for-each>
				</xsl:element>
			</xsl:if>
			<xsl:if test="normalize-space($subtitle) != ''">
				<xsl:element name="{$subtitle.element}">
					<xsl:for-each select="db:subtitle">
						<xsl:apply-templates select="node()" mode="body" />
					</xsl:for-each>
				</xsl:element>
			</xsl:if>
			<xsl:apply-templates select="node()" mode="body" />
		</xsl:element>
	</xsl:template>

	<!-- Un-titled block output -->
	<xsl:template name="html.block">
		<xsl:param name="element" select="'div'" />
		<xsl:param name="class" />
		<xsl:param name="role" select="@role" />
		<xsl:param name="id" select="normalize-space(@xml:id)" />		
		<xsl:param name="kind" select="local-name()" />
		<xsl:variable name="classes" select="normalize-space(concat($class, ' ', $role, ' ', $kind))" />
		<xsl:element name="{$element}">
			<xsl:if test="$id != ''"><xsl:attribute name="id"><xsl:value-of select="$id" /></xsl:attribute></xsl:if>			
			<xsl:if test="$classes != ''"><xsl:attribute name="class"><xsl:value-of select="$classes" /></xsl:attribute></xsl:if> 
			<xsl:apply-templates select="node()" mode="body" />
		</xsl:element>
	</xsl:template>
	<!--<xsl:include href="inline.xsl" />-->
    <!-- Bibliography inlines -->

	<!-- <citation> -->
	<xsl:template match="//db:citation" mode="body">
		<xsl:text>[</xsl:text>
		<xsl:call-template name="html.inline">
			<xsl:with-param name="element">cite</xsl:with-param>
			<xsl:with-param name="kind" />
		</xsl:call-template>
		<xsl:text>]</xsl:text>
	</xsl:template>
	
	<!-- <citetitle> -->
	<xsl:template match="//db:citetitle" mode="body">
		<xsl:call-template name="html.inline">
			<xsl:with-param name="element">cite</xsl:with-param>
			<xsl:with-param name="kind" />
		</xsl:call-template>
	</xsl:template>
	
	<!-- Error inlines -->
	
	<!-- Graphic inlines -->
	
	<!-- GUI inlines -->
	
	<!-- <menuchoice> -->
	<xsl:template match="//db:menuchoice" mode="body">
		<xsl:element name="kbd">
			<xsl:call-template name="html.inline.attrs" />
			<xsl:for-each select="node()">
				<xsl:choose>
					<xsl:when test="namespace-uri() = 'http://docbook.org/ns' and local-name() = 'shortcut'" />
					<xsl:when test="self::*">
						<xsl:apply-templates select="." mode="body" />
						<xsl:if test="position() != last()">
							<xsl:text> → </xsl:text>
						</xsl:if>
					</xsl:when>
					<xsl:otherwise>
						<xsl:apply-templates mode="body" />
					</xsl:otherwise>
				</xsl:choose>
			</xsl:for-each>
			<xsl:for-each select="//db:shortcut">
				<xsl:element name="samp">
					<xsl:attribute name="class">shortcut</xsl:attribute>
					<xsl:text> (</xsl:text>
					<xsl:apply-templates select="node()" mode="body" />
					<xsl:text>)</xsl:text>
				</xsl:element>
			</xsl:for-each>
		</xsl:element>
	</xsl:template>

	<!-- <guimenu>, <guimenuitem>, <guisubmenu>, <guibutton>, <guiicon>, <guilabel>, <mousebutton> -->
	<xsl:template match="//db:guimenu|//db:guimenuitem|//db:guisubmenu|//db:guibutton|//db:guiicon|//db:guilabel|//db:mousebutton" mode="body">
		<xsl:element name="kbd">
			<xsl:call-template name="html.inline.attrs" />
			<xsl:element name="samp">
				<xsl:apply-templates select="node()" mode="body" />
			</xsl:element>
		</xsl:element>
	</xsl:template>
		
	<!-- Indexing inlines -->
	
	<!-- Keyboard inlines -->
	
	<!-- Linking inlines -->
	
	<!-- <anchor> -->
	<xsl:template match="//db:anchor" mode="body">
		<xsl:call-template name="html.inline">
			<xsl:with-param name="element">a</xsl:with-param>
			<xsl:with-param name="kind" />
		</xsl:call-template>
	</xsl:template>
	
	<!-- <link> -->
	<xsl:template match="//db:link" mode="body">
		<xsl:element name="a">
			<xsl:attribute name="href"><xsl:value-of select="@xlink:href" /></xsl:attribute>
			<xsl:call-template name="html.inline.attrs"><xsl:with-param name="kind" /></xsl:call-template>
			<xsl:apply-templates select="node()" mode="body" />
		</xsl:element>
	</xsl:template>

	<!-- <olink> -->

	<!-- <xref> -->
	
	<!-- <biblioref -->
	
	<!-- Markup inlines -->

	<!-- <code> -->
	<xsl:template match="//db:code" mode="body">
		<xsl:call-template name="html.inline">
			<xsl:with-param name="element">code</xsl:with-param>
			<xsl:with-param name="kind" />
		</xsl:call-template>
	</xsl:template>
	
	<!-- <markup>, <constant>, <symbol>, <token>, <literal>, <uri> -->
	<xsl:template match="//db:markup|//db:constant|//db:symbol|//db:token|//db:literal|//db:uri" mode="body">
		<xsl:call-template name="html.inline">
			<xsl:with-param name="element">code</xsl:with-param>
		</xsl:call-template>
	</xsl:template>
	
	<!-- <email> -->
	<xsl:template match="//db:email" mode="body">
		<xsl:call-template name="html.inline">
			<xsl:with-param name="element">span</xsl:with-param>
		</xsl:call-template>
	</xsl:template>	
	
	<!-- <tag> -->
	<xsl:template match="//db:tag" mode="body">
		<xsl:choose>
			<xsl:when test="attribute::class='element' or attribute::class='starttag'" mode="body">
				<xsl:call-template name="html.inline">
		  			<xsl:with-param name="element">code</xsl:with-param>
					<xsl:with-param name="prefix">&lt;</xsl:with-param>
					<xsl:with-param name="suffix">&gt;</xsl:with-param>
		  		</xsl:call-template>
			</xsl:when>
			<xsl:when test="attribute::class='emptytag'">
				<xsl:call-template name="html.inline">
		  			<xsl:with-param name="element">code</xsl:with-param>
					<xsl:with-param name="prefix">&lt;</xsl:with-param>
					<xsl:with-param name="suffix"> /&gt;</xsl:with-param>
		  		</xsl:call-template>
			</xsl:when>
			<xsl:when test="attribute::class='endtag'">
				<xsl:call-template name="html.inline">
		  			<xsl:with-param name="element">code</xsl:with-param>
					<xsl:with-param name="prefix">&lt;/</xsl:with-param>
					<xsl:with-param name="suffix">&gt;</xsl:with-param>
		  		</xsl:call-template>
			</xsl:when>
			<xsl:when test="attribute::class='genentity'">
				<xsl:call-template name="html.inline">
		  			<xsl:with-param name="element">code</xsl:with-param>
					<xsl:with-param name="prefix">&amp;</xsl:with-param>
					<xsl:with-param name="suffix">;</xsl:with-param>
		  		</xsl:call-template>
			</xsl:when>
			<xsl:when test="attribute::class='numcharref'">
				<xsl:call-template name="html.inline">
		  			<xsl:with-param name="element">code</xsl:with-param>
					<xsl:with-param name="prefix">&amp;#</xsl:with-param>
					<xsl:with-param name="suffix">;</xsl:with-param>
		  		</xsl:call-template>
			</xsl:when>
			<xsl:when test="attribute::class='comment'">
				<xsl:call-template name="html.inline">
		  			<xsl:with-param name="element">code</xsl:with-param>
					<xsl:with-param name="prefix">&lt;!--</xsl:with-param>
					<xsl:with-param name="suffix">--&gt;</xsl:with-param>
		  		</xsl:call-template>
			</xsl:when>
			<xsl:when test="attribute::class='xmlpi' or attribute::class='pi'">
				<xsl:call-template name="html.inline">
		  			<xsl:with-param name="element">code</xsl:with-param>
					<xsl:with-param name="prefix">&lt;?</xsl:with-param>
					<xsl:with-param name="suffix">&gt;</xsl:with-param>
		  		</xsl:call-template>
			</xsl:when>
			<xsl:when test="attribute::class='attvalue'">
				<xsl:call-template name="html.inline">
		  			<xsl:with-param name="element">code</xsl:with-param>
					<xsl:with-param name="prefix">"</xsl:with-param>
					<xsl:with-param name="suffix">"</xsl:with-param>
		  		</xsl:call-template>
			</xsl:when>			
			<xsl:otherwise>
				<xsl:call-template name="html.inline">
		  			<xsl:with-param name="element">code</xsl:with-param>
				</xsl:call-template>
			</xsl:otherwise>
		</xsl:choose>
	</xsl:template>
	
	<!-- Math inlines -->
	
	<!-- Object-oriented programming inlines -->
	
	<!-- Operating system inlines -->
	
	<!-- <filename>, <command>, <envvar> -->
	<xsl:template match="//db:filename|//db:envvar" mode="body">
		<xsl:call-template name="html.inline">
			<xsl:with-param name="element">code</xsl:with-param>
		</xsl:call-template>
	</xsl:template>
	
	<!-- <command> -->
	<xsl:template match="//db:command" mode="body">
		<xsl:call-template name="html.inline">
			<xsl:with-param name="element">kbd</xsl:with-param>
		</xsl:call-template>
	</xsl:template>
		
	<!-- <userinput> -->
	<xsl:template match="//db:userinput" mode="body">
		<xsl:call-template name="html.inline">
			<xsl:with-param name="element">kbd</xsl:with-param>
			<xsl:with-param name="kind" />
		</xsl:call-template>
	</xsl:template>
	
	<!-- <prompt> -->
	<xsl:template match="//db:prompt" mode="body">
		<xsl:call-template name="html.inline">
			<xsl:with-param name="element">samp</xsl:with-param>
			<xsl:with-param name="class">output</xsl:with-param>
		</xsl:call-template>
	</xsl:template>
	
	<!-- <computeroutput> -->
	<xsl:template match="//db:computeroutput" mode="body">
		<xsl:call-template name="html.inline">
			<xsl:with-param name="element">samp</xsl:with-param>
			<xsl:with-param name="kind">output</xsl:with-param>
		</xsl:call-template>
	</xsl:template>
	
	<!-- Product inlines -->
	
	<!-- <application>, <database>, <hardware>, <productname>, <productnumber>, <trademark> -->
	
	
	<!-- Programming inlines -->
	
	<!-- <classname>, <exceptionname>, <function>, <initializer>, <interfacename>,
	   - <methodname>, <modifier>, <ooclass>, <ooexception>, <oointerface>,
	   - <parameter>, <returnvalue>, <type>
	   -->
	<xsl:template match="//db:classname|//db:exceptionname|//db:function|//db:initializer|//db:interfacename|//db:methodname|//db:modifier|//db:ooclass|//db:ooexception|//db:oointerface|//db:parameter|//db:returnvalue|//db:type" mode="body">
		<xsl:call-template name="html.inline">
			<xsl:with-param name="element">code</xsl:with-param>
		</xsl:call-template>
	</xsl:template>
	
	<!-- <varname> -->
	<xsl:template match="//db:varname" mode="body">
		<xsl:call-template name="html.inline">
			<xsl:with-param name="element">var</xsl:with-param>
			<xsl:with-param name="kind" />
		</xsl:call-template>
	</xsl:template>
		
	
	<!-- Publishing inlines -->
	
	<!-- <abbrev> and <acronym> -->
	<xsl:template match="//db:abbrev|//db:acronym" mode="body">
		<xsl:variable name="title"><xsl:copy-of select="db:alt" /></xsl:variable>
		<xsl:call-template name="html.inline">
			<xsl:with-param name="element">abbr</xsl:with-param>
			<xsl:with-param name="kind" />
			<xsl:with-param name="title"><xsl:value-of select="$title" /></xsl:with-param>
		</xsl:call-template>
	</xsl:template>
	
	<!-- <emphasis> -->
	<xsl:template match="//db:emphasis" mode="body">
		<xsl:variable name="element">
			<xsl:choose>
				<xsl:when test="attribute::role='strong'">strong</xsl:when>
				<xsl:otherwise>em</xsl:otherwise>
			</xsl:choose>
		</xsl:variable>
		<xsl:call-template name="html.inline">
			<xsl:with-param name="element" select="normalize-space($element)" />
			<xsl:with-param name="kind" />
		</xsl:call-template>
	</xsl:template>
	
	<!-- <phrase> -->
	<xsl:template match="//db:phrase" mode="body">
		<xsl:call-template name="html.inline">
			<xsl:with-param name="element">span</xsl:with-param>
			<xsl:with-param name="kind" />
		</xsl:call-template>
	</xsl:template>

	<!-- <foreignphrase>, <firstterm> -->
	<xsl:template match="//db:foreignphrase|//db:firstterm" mode="body">
		<xsl:call-template name="html.inline">
			<xsl:with-param name="element">i</xsl:with-param>
		</xsl:call-template>
	</xsl:template>
	
	<!-- Technical inlines -->	
	
	<!-- Ubiquitious inlines -->
	
	<!-- <remark> -->
	<xsl:template match="//db:remark" mode="body">
		<xsl:call-template name="html.inline">
			<xsl:with-param name="element">i</xsl:with-param>
		</xsl:call-template>
	</xsl:template>
	
	<!-- People -->
	
	<xsl:template match="//db:personname" mode="body">
		<xsl:if test="db:firstname|db:surname">			
			<xsl:if test="db:firstname">
				<xsl:apply-templates select="db:firstname" mode="body" />
				<xsl:if test="db:surname"><xsl:text> </xsl:text></xsl:if>
			</xsl:if>
			<xsl:if test="db:surname">
				<xsl:apply-templates select="db:surname" mode="body" />
			</xsl:if>
			<xsl:if test="db:affiliation">
				<xsl:text>, </xsl:text>
			</xsl:if>
		</xsl:if>
		<xsl:if test="db:affiliation">
			<xsl:if test="db:affiliation/db:org">
				<xsl:choose>
					<xsl:when test="db:affiliation/db:org/db:uri">
						<a>
							<xsl:attribute name="href"><xsl:value-of select="db:affiliation/db:org/db:uri" /></xsl:attribute>
							<xsl:apply-templates select="db:affiliation/db:org/db:orgname" mode="body" />
						</a>
					</xsl:when>
					<xsl:otherwise>
						<xsl:apply-templates select="db:affiliation/db:org/db:orgname" mode="body" />
					</xsl:otherwise>
				</xsl:choose>
			</xsl:if>
		</xsl:if>
	</xsl:template>
	
	<!-- Inline element output -->
	<xsl:template match="//db:alt" mode="body"/>
	
	<xsl:template name="html.inline">
		<xsl:param name="element" select="'span'" />
		<xsl:param name="class" />
		<xsl:param name="role" select="@role" />
		<xsl:param name="id" select="normalize-space(@xml:id)" />		
		<xsl:param name="kind" select="local-name()" />
		<xsl:param name="prefix" />
		<xsl:param name="suffix" />
		<xsl:param name="title" />
		<xsl:element name="{$element}">
			<xsl:call-template name="html.inline.attrs">
				<xsl:with-param name="class" select="$class" />
				<xsl:with-param name="role" select="$role" />
				<xsl:with-param name="id" select="$id" />
				<xsl:with-param name="kind" select="$kind" />
				<xsl:with-param name="title" select="$title" />
			</xsl:call-template>
			<xsl:copy-of select="$prefix" /><xsl:apply-templates select="node()" mode="body" /><xsl:copy-of select="$suffix" />
		</xsl:element>
	</xsl:template>
	
	<xsl:template name="html.inline.attrs">
		<xsl:param name="class" />
		<xsl:param name="role" select="@role" />
		<xsl:param name="id" select="normalize-space(@xml:id)" />		
		<xsl:param name="kind" select="local-name()" />
		<xsl:param name="title" />
		<xsl:variable name="classes" select="normalize-space(concat($class, ' ', $role, ' ', $kind))" />
		<xsl:if test="$id != ''"><xsl:attribute name="id"><xsl:value-of select="$id" /></xsl:attribute></xsl:if>			
		<xsl:if test="$classes != ''"><xsl:attribute name="class"><xsl:value-of select="$classes" /></xsl:attribute></xsl:if> 
		<xsl:if test="normalize-space($title) != ''"><xsl:attribute name="title"><xsl:value-of select="$title" /></xsl:attribute></xsl:if>
	</xsl:template>	
	<!--<xsl:include href="doc.xsl" />-->
    <!-- Root element switch -->
	<xsl:template match="/*">
		<xsl:variable name="doc.title">
			<xsl:choose>
				<xsl:when test="/db:refentry">
					<xsl:copy-of select="db:refmeta/db:refentrytitle" />
				</xsl:when>
				<xsl:otherwise>
					<xsl:copy-of select="db:title" />
				</xsl:otherwise>
			</xsl:choose>
		</xsl:variable>
		<xsl:variable name="doc.subtitle"><xsl:copy-of select="/*/db:subtitle" /></xsl:variable>
		<xsl:call-template name="html.doctype" />
		<xsl:text disable-output-escaping="yes"><![CDATA[
<html>
	<head>
]]></xsl:text>
		<xsl:call-template name="html.meta" />
		<title><xsl:value-of select="$doc.title" /></title>
		<xsl:call-template name="html.links" />
		<xsl:call-template name="html.scripts" />
		<xsl:text disable-output-escaping="yes"><![CDATA[
	</head>
]]></xsl:text>
		<xsl:choose>
			<xsl:when test="/db:book">
				<xsl:message><xsl:text>Processing a DocBook &lt;book&gt;</xsl:text></xsl:message>
				<xsl:call-template name="html.body">
					<xsl:with-param name="kind">book</xsl:with-param>
					<xsl:with-param name="title" select="$doc.title" />
					<xsl:with-param name="subtitle" select="$doc.subtitle" />
				</xsl:call-template>
			</xsl:when>
			<xsl:when test="/db:refentry">
				<xsl:message><xsl:text>Processing a DocBook &lt;refentry&gt;</xsl:text></xsl:message>
				<xsl:call-template name="html.body">
					<xsl:with-param name="kind">refentry</xsl:with-param>
					<xsl:with-param name="title" select="$doc.title" />
					<xsl:with-param name="subtitle" select="$doc.subtitle" />
				</xsl:call-template>
			</xsl:when>
			<xsl:otherwise>
				<xsl:message terminate="yes">
					<xsl:text>Unsupported root element</xsl:text>
				</xsl:message>
			</xsl:otherwise>
		</xsl:choose>
		<xsl:text disable-output-escaping="yes"><![CDATA[
</html>
]]></xsl:text>
	</xsl:template>
	
	<!-- Generate the HTML <body> -->
	<xsl:template name="html.body">
		<xsl:param name="kind" />
		<xsl:param name="title" />
		<xsl:param name="subtitle" />
		<body class="{$kind}">
			<xsl:call-template name="html.frontmatter">
				<xsl:with-param name="title" select="$title" />
				<xsl:with-param name="subtitle" select="$subtitle" />
			</xsl:call-template>
			<article>
				<xsl:apply-templates select="node()" mode="body" />
			</article>
			<xsl:call-template name="html.backmatter" />
		</body>
	</xsl:template>
	
	<!-- Generate any frontmatter -->
	<xsl:template name="html.frontmatter">
		<xsl:param name="title" />
		<xsl:param name="subtitle" />
		<header>
			<xsl:call-template name="html.globalnav" />
			<xsl:call-template name="html.masthead" />
			<h1><xsl:value-of select="$title" /></h1>
			<xsl:if test="normalize-space($subtitle) != ''">
				<h2><xsl:value-of select="$subtitle" /></h2>
			</xsl:if>
			<xsl:for-each select="db:info/db:edition"><p class="edition"><xsl:apply-templates select="node()" mode="body" /></p></xsl:for-each>
			<xsl:if test="db:info/db:editor">
				<p class="editor">
					<xsl:text>Edited by </xsl:text>
					<xsl:for-each select="db:info/db:editor">
						<xsl:if test="position() != 1"><xsl:text>, </xsl:text></xsl:if>
						<xsl:call-template name="html.person" />
					</xsl:for-each>
					<xsl:text>.</xsl:text>
				</p>
			</xsl:if>
		</header>
		<!-- Use a series of xsl:for-each stanzas to output in a specific order -->
		<xsl:for-each select="db:info/db:legalnotice"><xsl:call-template name="html.titleblock" /></xsl:for-each>
		<xsl:for-each select="db:preface"><xsl:call-template name="html.titleblock" /></xsl:for-each>
		<xsl:for-each select="db:acknowledgements"><xsl:call-template name="html.titleblock" /></xsl:for-each>
		<xsl:call-template name="html.toc" />
	</xsl:template>
	
	<!-- Emit a person's name -->
	<xsl:template name="html.person">
		<xsl:if test="db:personname">
			<xsl:apply-templates select="db:personname" mode="body" />
		</xsl:if>
	</xsl:template>
	
	<!-- Generate any backmatter -->
	<xsl:template name="html.backmatter">
		<!-- Use a series of xsl:for-each stanzas to output in a specific order -->
		<xsl:for-each select="db:glossary"><xsl:call-template name="html.titleblock" /></xsl:for-each>
		<xsl:for-each select="db:bibliography"><xsl:call-template name="html.titleblock" /></xsl:for-each>
		<xsl:for-each select="db:index"><xsl:call-template name="html.titleblock" /></xsl:for-each>
		<xsl:for-each select="db:colophon"><xsl:call-template name="html.titleblock" /></xsl:for-each>
	</xsl:template>		
		
	<!-- Don't emit certain elements -->
	<xsl:template match="//db:title" mode="body" />
	<xsl:template match="//db:subtitle" mode="body" />
	<xsl:template match="//db:info" mode="body" />
	<xsl:template match="//db:refmeta" mode="body" />
	<xsl:template match="//db:legalnotice|//db:preface|//db:acknowledgements|//db:toc|//db:dedication" mode="body" />
	<xsl:template match="//db:appendix|//db:bibliography|//db:colophon|//db:glossary|//db:index" mode="body" />
	<!--<xsl:include href="toc.xsl" />	-->
    <!-- Generate the table of contents -->
	<xsl:template name="html.toc">
		<xsl:choose>
			<xsl:when test="/node()/db:toc">
				<xsl:call-template name="html.explicittoc" />
			</xsl:when>
			<xsl:otherwise>
				<xsl:call-template name="html.autotoc" />
			</xsl:otherwise>
		</xsl:choose>
	</xsl:template>
	
	<!-- Emit a provided table of contents -->
	<xsl:template name="html.explicittoc">
		<nav class="toc">
			<h1>Table of contents</h1>
			<ol>
				<xsl:apply-templates select="/node()/db:toc/*" mode="body" />
			</ol>
		</nav>
	</xsl:template>		
	
	<xsl:template match="//db:tocentry" mode="body">
		<xsl:param name="role" select="@role" />		
		<li>
			<xsl:if test="normalize-space($role) != ''"><xsl:attribute name="class"><xsl:value-of select="$role" /></xsl:attribute></xsl:if> 
			<xsl:apply-templates select="node()" mode="body" />
		</li>
	</xsl:template>

	<xsl:template match="//db:tocdiv" mode="body">
		<xsl:param name="role" select="@role" />		
		<xsl:param name="title" select="db:title" />
		<xsl:choose>
			<xsl:when test="parent::db:toc">
					<li>
						<xsl:if test="normalize-space($role) != ''"><xsl:attribute name="class"><xsl:value-of select="$role" /></xsl:attribute></xsl:if>
						<xsl:value-of select="$title" />
						<ol>
							<xsl:apply-templates select="*" mode="body" />
						</ol>
					</li>
			</xsl:when>
			<xsl:otherwise>		
				<xsl:value-of select="$title" />
				<ol>
					<xsl:apply-templates select="*" mode="body" />
				</ol>
			</xsl:otherwise>
		</xsl:choose>
	</xsl:template>
	
	<!-- Automatically generate a table of contents when db:book is the root -->
	<xsl:template name="html.autotoc">
		<xsl:for-each select="/db:book">
			<xsl:choose>
				<xsl:when test="db:preface|db:acknowledgements|db:section|db:chapter|db:part|db:refsection|db:refsect1|db:refsect2|db:refsect3|db:refsect4|db:reference|db:refsynopsisdiv|db:refentry|db:glossary|db:bibliography|db:index|db:colophon">
					<nav class="toc">
						<h1>Table of contents</h1>
						<ol>
							<xsl:for-each select="db:preface|db:acknowledgements">
								<xsl:call-template name="html.toc.entry">
									<xsl:with-param name="class" select="'frontmatter'" />
								</xsl:call-template>
							</xsl:for-each>
							<xsl:for-each select="db:section|db:chapter|db:part|db:refsection|db:refsect1|db:refsect2|db:refsect3|db:refsect4|db:reference|db:refsynopsisdiv|db:refentry">
								<xsl:call-template name="html.toc.entry" />
							</xsl:for-each>
							<xsl:for-each select="db:glossary|db:bibliography|db:index|db:colophon">
								<xsl:call-template name="html.toc.entry">
									<xsl:with-param name="class" select="'backmatter'" />
								</xsl:call-template>
							</xsl:for-each>
							
						</ol>			
					</nav>
				</xsl:when>
			</xsl:choose>
		</xsl:for-each>
	</xsl:template>
	
	<xsl:template name="html.toc.list">
		<xsl:param name="class" />	
		<xsl:choose>
			<xsl:when test="db:section|db:chapter|db:part|db:refsection|db:refsect1|db:refsect2|db:refsect3|db:refsect4|db:reference|db:refsynopsisdiv|db:refentry">
				<ol>
					<xsl:for-each select="db:section|db:chapter|db:part|db:refsection|db:refsect1|db:refsect2|db:refsect3|db:refsect4|db:reference|db:refsynopsisdiv|db:refentry">
						<xsl:call-template name="html.toc.entry">
							<xsl:with-param name="class" select="$class" />
						</xsl:call-template>
					</xsl:for-each>
				</ol>
			</xsl:when>
			<xsl:otherwise>
				<xsl:for-each select="*">
					<xsl:call-template name="html.toc.list">
						<xsl:with-param name="class" select="$class" />
					</xsl:call-template>
				</xsl:for-each>
			</xsl:otherwise>
		</xsl:choose>
	</xsl:template>
	
	<xsl:template name="html.toc.entry">
		<xsl:param name="class" />	
		<xsl:variable name="title">
			<xsl:choose>
				<xsl:when test="db:title">
					<xsl:copy-of select="db:title" />
				</xsl:when>
				<xsl:when test="db:refmeta/db:refentrytitle">
					<xsl:copy-of select="db:refmeta/db:refentrytitle" />
				</xsl:when>
				<xsl:when test="local-name() = 'preface'">
					<xsl:text>Preface</xsl:text>
				</xsl:when>
				<xsl:when test="local-name() = 'acknowledgements'">
					<xsl:text>Acknowledgements</xsl:text>
				</xsl:when>
				<xsl:when test="local-name() = 'index'">
					<xsl:text>Index</xsl:text>
				</xsl:when>
				<xsl:when test="local-name() = 'glossary'">
					<xsl:text>Glossary</xsl:text>
				</xsl:when>
				<xsl:when test="local-name() = 'bibliography'">
					<xsl:text>Bibliography</xsl:text>
				</xsl:when>
				<xsl:when test="local-name() = 'colophon'">
					<xsl:text>Colophon</xsl:text>
				</xsl:when>				
			</xsl:choose>
		</xsl:variable>
		<xsl:variable name="id"><xsl:copy-of select="normalize-space(@xml:id)" /></xsl:variable>
		<xsl:choose>
			<xsl:when test="normalize-space($title) != ''">
				<li>
					<xsl:if test="normalize-space($class) != ''">
						<xsl:attribute name="class">
							<xsl:value-of select="$class" />
						</xsl:attribute>
					</xsl:if>
					<xsl:choose>
						<xsl:when test="normalize-space($id) != ''">
							<a>
								<xsl:attribute name="href"><xsl:value-of select="concat('#', $id)" /></xsl:attribute>
								<xsl:value-of select="$title" />
							</a>
						</xsl:when>
						<xsl:otherwise>
							<xsl:value-of select="$title" />
						</xsl:otherwise>						
					</xsl:choose>
					<xsl:call-template name="html.toc.list">
						<xsl:with-param name="class" select="$class" />
					</xsl:call-template>						
				</li>
			</xsl:when>
			<xsl:otherwise>
				<xsl:for-each select="*">
					<xsl:call-template name="html.toc.list">
						<xsl:with-param name="class" select="$class" />
					</xsl:call-template>					
				</xsl:for-each>
			</xsl:otherwise>
		</xsl:choose>
	</xsl:template>

	<!-- HTML5 doctype -->
	<xsl:template name="html.doctype">
		<xsl:text disable-output-escaping="yes"><![CDATA[<!DOCTYPE html>]]></xsl:text>
	</xsl:template>
	
	<!-- HTML5 meta elements -->
	<xsl:template name="html.meta">
		<xsl:text disable-output-escaping="yes"><![CDATA[
			<meta http-equiv="X-UA-Compatible" content="IE=edge" />
			<meta charset="utf-8" />
			<!--[if lt IE 9]><script>document.createElement('header');document.createElement('nav');document.createElement('section');document.createElement('article');document.createElement('aside');document.createElement('footer');</script><![endif]-->
		]]></xsl:text>
	</xsl:template>

	<!-- HTML5 stylesheets -->
	<xsl:param name="html.linksfile" select="''" />
	<xsl:param name="html.ie78css" select="''" />
	<xsl:template name="html.links">
	  <xsl:if test="normalize-space($html.linksfile) != ''">
		<xsl:call-template name="include-xml">
		  <xsl:with-param name="uri" select="$html.linksfile" />
		</xsl:call-template>
	  </xsl:if>
	  <xsl:if test="normalize-space($html.ie78css) != ''">
		  <xsl:text disable-output-escaping="yes"><![CDATA[<!--[if lt IE 9]><link rel="stylesheet" type="text/css" href="]]></xsl:text>
		  <xsl:value-of select="$html.ie78css" />
		  <xsl:text disable-output-escaping="yes"><![CDATA[" /><![endif]-->]]></xsl:text>
	  </xsl:if>
	</xsl:template>

	<!-- HTML5 scripts -->
	<xsl:template name="html.scripts" />

	<!-- Global navigation -->
	<xsl:param name="html.navfile" select="''" />
	<xsl:template name="html.globalnav">
		<xsl:if test="normalize-space($html.navfile) != ''">
			<nav class="global"><div class="inner">
				<xsl:call-template name="include-xml">
					<xsl:with-param name="uri" select="$html.navfile" />
				</xsl:call-template>
			</div></nav>			
		</xsl:if>
	</xsl:template>

	<!-- Masthead -->
	<xsl:template name="html.masthead">
		<xsl:text disable-output-escaping="yes"><![CDATA[
		<div class="masthead"></div>
		]]></xsl:text>
	</xsl:template>

	<!-- Utilities -->
	<xsl:template name="include-xml">
	  <xsl:param name="uri" />
	  <xsl:message>Including source file <xsl:value-of select="$uri" /></xsl:message>
	  <xsl:copy-of select="document($uri)/include/*" disable-output-escaping="yes" />
	</xsl:template>
	
</xsl:stylesheet>
