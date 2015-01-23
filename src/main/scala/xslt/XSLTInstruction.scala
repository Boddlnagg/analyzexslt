package xslt

import xpath.XPathExpr

/** Base class for XSLT instructions */
abstract class XSLTInstruction

/** Instruction to create an element node (see XSLT spec section 7.1.1 and 7.1.2)
  *
  * @param name the name of the element, as an attribute value template (namespaces are not implemented)
  * @param content the instructions used to generate the attributes and children of the element
  */
case class CreateElementInstruction(name: List[Either[String, XPathExpr]], content: Seq[XSLTInstruction]) extends XSLTInstruction

/** Set attribute instruction, created using the &lt;xsl:attribute&gt; element (see XSLT spec section 7.1.3)
  * or from attributes in literal elements.
  *
  * @param name the name of the attribute, as an attribute value template
  * @param value the instructions used to compute the value of the attribute
  */
case class SetAttributeInstruction(name: List[Either[String, XPathExpr]], value: Seq[XSLTInstruction]) extends XSLTInstruction

/** Instruction to create a text node (see XSLT spec section 7.2)
  *
  * @param text the text of the text node
  */
case class CreateTextInstruction(text: String) extends XSLTInstruction

/** Create an XML comment in the output (see XSLT spec section 7.4)
  *
  * @param content the template that is instantiated to generate the content of the comment
  */
case class CreateCommentInstruction(content: Seq[XSLTInstruction]) extends XSLTInstruction

/** Apply templates instruction, created using the &lt;xsl:apply-templates&gt; element (see XSLT spec 5.4)
  *
  * @param select an optional XPath expression (must return a node-set) to select a different set of nodes instead of the children
  * @param params the parameters for the template instantiation, as defined with &lt;xsl:with-param&gt;
  */
case class ApplyTemplatesInstruction(select: Option[XPathExpr], mode: Option[String], params: Map[String, Either[XPathExpr, Seq[XSLTInstruction]]] = Map()) extends XSLTInstruction

/** Call template instruction, created using the &lt;xsl:call-template&gt; element (see XSLT spec section 6)
  *
  * @param name the name of the called template
  * @param params the parameters for the template instantiation, as defined with &lt;xsl:with-param&gt;
  */
case class CallTemplateInstruction(name: String, params: Map[String, Either[XPathExpr, Seq[XSLTInstruction]]] = Map()) extends XSLTInstruction

/** Copy-of instruction, created using the &lt;xsl:copy-of&gt; or &lt;xsl:value-of&gt; elements (see XSLT spec sections 7.6.1 and 11.3)
  * For &lt;xsl:value-of&gt;, the result will be converted to a string by wrapping the select expression with a call to the string() function.
  *
  * @param select The XPath expression to compute the value that is copied (either a node-set or any other node
  *               which is then first converted to a string and results in a text node)
  */
case class CopyOfInstruction(select: XPathExpr) extends XSLTInstruction

/** Copy instruction, created using the &lt;xsl:copy&gt; element (see XSLT spec sections 7.5)
  * This copies the current node, but without any children or attributes.
  *
  * @param content the template that is instantiated to generate the content (children) of the resulting node
  */
case class CopyInstruction(content: Seq[XSLTInstruction]) extends XSLTInstruction

/** Choose instruction, created using the &lt;xsl:choose&gt; or the &lt;xsl:if&gt; elements (see XSLT spec sections 9.1 and 9.2)
  *
  * @param branches a list of branches, where each branch consists of a test-expression and a sequence of instructions
  * @param otherwise the instructions that are evaluated when no previous branch matches
  */
case class ChooseInstruction(branches: List[(XPathExpr, Seq[XSLTInstruction])], otherwise: Seq[XSLTInstruction]) extends XSLTInstruction

/** For-each instruction, created using the &lt;xsl:for-each&gt; element (see XSLT spec section 8)
  *
  * @param select an XPath expression (must return a node-set) that specifies what nodes the content should be instantiated for
  * @param content the instructions that are evaluated for each of the selected node
  */
case class ForEachInstruction(select: XPathExpr, content: Seq[XSLTInstruction]) extends XSLTInstruction

/** Variable definitions, created using the &lt;xsl:variable&gt; element (see XSLT spec section 11)
  *
  * @param name the name of the variable
  * @param value either an XPath expression used to compute the value or a template for a result tree fragment
  */
case class VariableDefinitionInstruction(name: String, value: Either[XPathExpr, Seq[XSLTInstruction]]) extends XSLTInstruction

/** Number instruction, created using the &lt;xsl:number&gt; element (see XSLT spec section 8),
  * used for formatting numbers as strings and counting nodes in the source tree.
  *
  * This is only implemented as a dummy and therefore does not have parameters/content.
  */
case class NumberInstruction() extends XSLTInstruction