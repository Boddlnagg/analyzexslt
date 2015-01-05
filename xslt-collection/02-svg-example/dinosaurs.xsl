<?xml version="1.0" ?>
<!-- 
    Begin the stylesheet. see the first stylesheet in this chapter
    for comments.
-->
<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">

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

</xsl:stylesheet>

